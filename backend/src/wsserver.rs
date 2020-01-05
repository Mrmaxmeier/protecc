use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Weak};

use futures::channel::mpsc;
use futures::future::Future;
use futures::future::FutureExt;
use futures::sink::SinkExt;
use futures::StreamExt;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, oneshot, Mutex};
use tokio_tungstenite::tungstenite::Message;

use crate::database::Database;
use crate::incr_counter;
use crate::query;

#[derive(Serialize, Deserialize, Debug)]
struct RespFrame {
    id: u64,
    payload: ResponsePayload,
}

#[derive(Serialize, Deserialize, Debug)]
struct ReqFrame {
    id: u64,
    payload: RequestPayload,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum RequestPayload {
    Watch(StreamKind),
    Cancel,
    StepCursor(query::Cursor),
    Query2Cursor(query::Query),
    DoS(DebugDenialOfService),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum ResponsePayload {
    Counters(HashMap<String, u64>),
    Cursor(query::Cursor),
    Error(String),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum StreamKind {
    Counters,
    Query(query::Query),
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
#[serde(rename_all = "camelCase")]
enum DebugDenialOfService {
    HoldReadLock,
    HoldWriteLock,
    Panic,
    HoldAndPanic,
}

struct ConnectionHandler {
    db: Arc<Database>,
    cancel_chans: Mutex<HashMap<u64, Weak<Mutex<Option<oneshot::Sender<()>>>>>>, // TODO: refactor this
    stream_tx: mpsc::Sender<RespFrame>,
}

impl ConnectionHandler {
    async fn watch(&self, req_id: u64, kind: &StreamKind) {
        // {"id": 0, "payload": {"watch": "counters"}}
        // {"id": 0, "payload": "cancel"}
        match kind {
            StreamKind::Counters => {
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
            StreamKind::Query(..) => todo!(),
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
        if cursor.has_next() {
            // TODO: remove
            let mut buf = Vec::new();
            cursor.execute(&*self.db, &mut buf).await;
        }
        ResponsePayload::Cursor(cursor)
    }

    async fn gc_cancel_chans(&self) {
        let mut chans = self.cancel_chans.lock().await;
        chans.retain(|_, v| v.strong_count() > 0);
    }

    async fn await_cancel(self: Arc<Self>, id: u64) {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let tx = Arc::new(Mutex::new(Some(tx)));
        {
            self.cancel_chans
                .lock()
                .await
                .insert(id, Arc::downgrade(&tx));
        }
        let _ = rx.await;
    }

    async fn setup_cancellation<T: Future>(
        self: Arc<Self>,
        req: &ReqFrame,
        mut close_rx: broadcast::Receiver<()>,
        f: T,
    ) {
        futures::select! {
            _ = f.fuse() => {},
            _ = close_rx.recv().fuse() => {
                println!("req cancelled due to closed connection: {:?}", req)
            }
            _ = self.clone().await_cancel(req.id).fuse() => {
                println!("req cancelled due to cancel message: {:?}", req)
            }
        };
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

    async fn handle_req(self: Arc<Self>, req: ReqFrame, close_rx: broadcast::Receiver<()>) {
        if let RequestPayload::Cancel = req.payload {
            let cancel_chans = self.cancel_chans.lock().await;
            if let Some(tx) = cancel_chans.get(&req.id).and_then(|tx| tx.upgrade()) {
                let _ = tx.lock().await.take().unwrap().send(());
            }
            return;
        }

        // TODO(refactor)
        let self_ = self.clone();
        self.setup_cancellation(&req, close_rx, async {
            match &req.payload {
                RequestPayload::DoS(dos) => self_.dos(dos).await,
                RequestPayload::Watch(kind) => self_.watch(req.id, kind).await,
                RequestPayload::Query2Cursor(query) => {
                    self_.send_out(&req, self_.query2cursor(query)).await
                }
                _ => todo!(),
            };
        }).await;
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
        cancel_chans: tokio::sync::Mutex::new(HashMap::new()),
        stream_tx,
    });

    // NOTE: this is sort of redundant with cancel_chans.
    // It provides nice unwinding semantics though.
    let (close_tx, _) = broadcast::channel(1);

    let (mut write, read) = ws_stream.split();
    enum SelectKind {
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
                            tokio::spawn(
                                conn_handler.clone().handle_req(frame, close_tx.subscribe()),
                            );
                        } else {
                            let resp = RespFrame {
                                id: 0,
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
        conn_handler.gc_cancel_chans().await;
    }

    println!("WebSocket connection dropped: {}", addr);
    drop(write); // NOTE: this canceles all waiting / lock-bearing in-flight requests
}
