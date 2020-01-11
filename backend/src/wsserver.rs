use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

use futures::channel::mpsc;
use futures::future::Future;
use futures::future::FutureExt;
use futures::sink::SinkExt;
use futures::StreamExt;
use tokio::net::TcpStream;
use tokio::sync::{oneshot, Mutex};
use tokio_tungstenite::tungstenite::Message;

use crate::database::{Database, StreamID};
use crate::incr_counter;
use crate::query;
use crate::window::WindowHandle;

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct RespFrame {
    pub(crate) id: u64,
    pub(crate) payload: ResponsePayload,
}

#[derive(Serialize, Deserialize, Debug)]
struct ReqFrame {
    id: u64,
    payload: RequestPayload,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum RequestPayload {
    Watch(ResponseStreamKind),
    Cancel,
    StepCursor(query::Cursor),
    Query2Cursor(query::Query),
    DoS(DebugDenialOfService),
    WindowUpdate {
        id: u64,
        params: crate::window::WindowParameters,
    },
    FetchStreamPayload(StreamID),
    RegisterActor(crate::pipeline::PipelineRegistration),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ResponsePayload {
    Counters(HashMap<String, u64>),
    Configuration(crate::configuration::Configuration),
    Cursor(query::Cursor),
    CursorResult(query::Cursor, Vec<crate::database::Stream>, bool),
    Error(String),
    WindowUpdate(crate::window::WindowUpdate),
    StreamDetails(StreamDetails),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum ResponseStreamKind {
    Counters,
    Configuration,
    Window {
        index: query::QueryIndex,
        params: crate::window::WindowParameters,
    },
    StreamDetails(StreamID),
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
#[serde(rename_all = "camelCase")]
enum DebugDenialOfService {
    HoldReadLock,
    HoldWriteLock,
    Panic,
    HoldAndPanic,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StreamDetails {
    stream: crate::database::Stream,
    client_payload: Vec<u8>,
    server_payload: Vec<u8>,
}

struct ConnectionHandler {
    db: Arc<Database>,
    cancel_chans: Mutex<HashMap<u64, oneshot::Sender<()>>>,
    windows: Mutex<HashMap<u64, Arc<WindowHandle>>>,
    stream_tx: mpsc::Sender<RespFrame>,
}

impl ConnectionHandler {
    async fn watch(&self, req_id: u64, kind: &ResponseStreamKind) {
        // {"id": 0, "payload": {"watch": "counters"}}
        // {"id": 0, "payload": "cancel"}
        // {"id": 0, "payload": {"watch": {"window": {"kind": "all"}}}}
        match kind {
            ResponseStreamKind::Counters => {
                let mut out_stream = self.stream_tx.clone();
                let mut watcher = crate::counters::subscribe();
                let mut prev = HashMap::new();
                while let Some(counters) = watcher.recv().await {
                    let mut next = counters.as_hashmap();
                    next.retain(|k, v| v != prev.get(k).unwrap_or(&0));
                    out_stream
                        .send(RespFrame {
                            id: req_id,
                            payload: ResponsePayload::Counters(next),
                        })
                        .await
                        .unwrap();
                    prev = counters.as_hashmap();
                }
            }
            ResponseStreamKind::Configuration => {
                let mut out_stream = self.stream_tx.clone();
                let mut handle = self.db.configuration_handle.clone();
                while let Some(config) = handle.rx.recv().await {
                    out_stream
                        .send(RespFrame {
                            id: req_id,
                            payload: ResponsePayload::Configuration(config),
                        })
                        .await
                        .unwrap();
                }
            }
            ResponseStreamKind::Window { index, params } => {
                let (mut window, window_handle) =
                    crate::window::Window::new(index, self.db.clone()).await;
                window_handle.update(params.clone()).await;

                {
                    self.windows.lock().await.insert(req_id, window_handle);
                }

                window
                    .stream_results_to(req_id, self.stream_tx.clone())
                    .await;
            }
            ResponseStreamKind::StreamDetails(stream_id) => {
                let mut tx = self.stream_tx.clone();

                let db = self.db.clone();
                let stream = {
                    let streams = db.streams.read().await;
                    streams[stream_id.idx()].clone() // might panic, but that's ok
                };
                let client_payload = db
                    .datablob(stream.client_data_id)
                    .expect("couldn't find client payload");
                let server_payload = db
                    .datablob(stream.server_data_id)
                    .expect("couldn't find server payload");
                tx.send(RespFrame {
                    id: req_id,
                    payload: ResponsePayload::StreamDetails(StreamDetails {
                        stream,
                        client_payload,
                        server_payload,
                    }),
                })
                .await
                .unwrap();
                // TODO: stream tag updates?
            }
        }
    }

    async fn dos(&self, kind: &DebugDenialOfService) {
        // {"id": 0, "payload": {"doS": "holdWriteLock"}}
        // {"id": 1, "payload": {"doS": "holdWriteLock"}}
        // {"id": 0, "payload": "cancel"}
        // {"id": 1, "payload": "cancel"}
        use DebugDenialOfService::*;
        match kind {
            HoldReadLock => {
                println!("[DEBUG] thread is acquiring read lock");
                let _guard = self.db.streams.read().await;
                println!("[DEBUG] thread is holding read lock");
                let _guard = self.db.streams.write().await; // deadlock
            }
            HoldWriteLock => {
                println!("[DEBUG] thread is acquiring write lock");
                let _guard = self.db.streams.write().await;
                println!("[DEBUG] thread is holding write lock");
                let _guard = self.db.streams.read().await; // deadlock
            }
            Panic => panic!("{:?}", kind),
            HoldAndPanic => {
                let _guard = self.db.streams.read().await;
                panic!("{:?}", kind);
            }
        }
    }

    async fn query2cursor(&self, query: &query::Query) -> ResponsePayload {
        let cursor = query.clone().into_cursor(&*self.db).await;
        ResponsePayload::Cursor(cursor)
    }

    async fn step_cursor(&self, cursor: &query::Cursor) -> ResponsePayload {
        let mut buf = Vec::new();
        let cursor = cursor.execute(&*self.db, &mut buf).await;
        let has_next = cursor.has_next();
        ResponsePayload::CursorResult(cursor, buf, has_next)
    }

    async fn await_cancel(self: Arc<Self>, id: u64) {
        let (tx, rx) = tokio::sync::oneshot::channel();
        {
            self.cancel_chans.lock().await.insert(id, tx);
        }
        let _ = rx.await;
    }

    async fn setup_cancellation<T: Future>(self: Arc<Self>, req: &ReqFrame, f: T) {
        futures::select! {
            _ = f.fuse() => {},
            _ = self.clone().await_cancel(req.id).fuse() => {
                println!("req cancelled: {:?}", req);
                return;
            }
        };

        let mut cancel_chans = self.cancel_chans.lock().await;
        let _ = cancel_chans.remove(&req.id); // drop cancel channel if present
    }

    async fn send_out(&self, req: &ReqFrame, contents: impl Future<Output = ResponsePayload>) {
        let result = contents.await;
        self.stream_tx
            .clone()
            .send(RespFrame {
                id: req.id,
                payload: result,
            })
            .await
            .expect("unable to send out response frame");
    }

    async fn handle_req(self: Arc<Self>, req: ReqFrame) {
        tracyrs::zone!("ConnHandler::handle_req");
        if let RequestPayload::Cancel = req.payload {
            let mut cancel_chans = self.cancel_chans.lock().await;
            let _ = cancel_chans.remove(&req.id); // drop cancel channel if present
            return;
        }

        // TODO(refactor)
        let self_ = self.clone();
        self.setup_cancellation(&req, async {
            match &req.payload {
                RequestPayload::DoS(dos) => self_.dos(dos).await,
                RequestPayload::Watch(kind) => self_.watch(req.id, kind).await,
                RequestPayload::Query2Cursor(query) => {
                    self_.send_out(&req, self_.query2cursor(query)).await
                }
                RequestPayload::WindowUpdate { id, params } => {
                    let windows = self_.windows.lock().await;
                    if let Some(window) = windows.get(id) {
                        window.update(params.clone()).await;
                    }
                }
                RequestPayload::StepCursor(cursor) => {
                    self_.send_out(&req, self_.step_cursor(cursor)).await
                }
                _ => todo!(),
            };
        })
        .await;

        match &req.payload {
            RequestPayload::Watch(ResponseStreamKind::Window { .. }) => {
                let mut windows = self_.windows.lock().await;
                let _ = windows.remove(&req.id);
            }
            _ => { /* no cleanup */ }
        }
    }
}

pub(crate) async fn accept_connection(stream: TcpStream, database: Arc<Database>) {
    let addr = stream
        .peer_addr()
        .expect("connected streams should have a peer address");
    println!("Peer address: {}", addr);

    let ws_stream = tokio_tungstenite::accept_async(stream)
        .await
        .expect("Error during the websocket handshake occurred");

    println!("New WebSocket connection: {}", addr);
    incr_counter!(ws_connections);

    let (stream_tx, stream_rx) = mpsc::channel::<RespFrame>(8);

    let conn_handler = Arc::new(ConnectionHandler {
        db: database,
        cancel_chans: Mutex::new(HashMap::new()),
        windows: Mutex::new(HashMap::new()),
        stream_tx,
    });

    let (mut write, read) = ws_stream.split();
    enum SelectKind {
        // TODO(refactor) select!
        In(Result<Message, tokio_tungstenite::tungstenite::error::Error>),
        Out(RespFrame),
    }

    let mut read =
        futures::stream::select(read.map(SelectKind::In), stream_rx.map(SelectKind::Out));
    // TODO(upstream): why futures::channel::mpsc instread of tokio::sync::mpsc?
    // ^ why does tokio::sync::mpsc::Receiver not implement futures::Stream?

    while let Some(msg) = read.next().await {
        match msg {
            SelectKind::In(rmsg) => {
                incr_counter!(ws_rx);
                let msg = rmsg.expect("read.next().is_err()");
                match msg {
                    Message::Text(text) => {
                        let frame = serde_json::from_str::<ReqFrame>(&text);
                        if let Ok(frame) = frame {
                            dbg!(&frame);
                            tokio::spawn(conn_handler.clone().handle_req(frame));
                        } else {
                            let resp = RespFrame {
                                id: 0x4141_4141,
                                payload: ResponsePayload::Error(format!("{:?}", frame)),
                            };
                            write
                                .send(Message::Text(serde_json::to_string(&resp).unwrap()))
                                .await
                                .expect("failed to send");
                        }
                    }
                    Message::Close(..) => {
                        let _ = write.send(msg).await;
                        break;
                    }
                    Message::Ping(_) => {} // handled by tungstenite
                    _ => panic!("unhandled msg frame {:?}", msg),
                }
            }
            SelectKind::Out(msg) => {
                incr_counter!(ws_tx);
                write
                    .send(Message::Text(serde_json::to_string(&msg).unwrap()))
                    .await
                    .expect("failed to send");
            }
        }
    }

    println!("WebSocket connection dropped: {}", addr);
    // NOTE: this aborts all in-flight requests and releases all held locks.
    conn_handler.cancel_chans.lock().await.drain();
}
