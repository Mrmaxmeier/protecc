use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Weak};

use futures::channel::mpsc;
use futures::future::FutureExt;
use futures::sink::SinkExt;
use futures::StreamExt;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, oneshot, Mutex};
use tokio_tungstenite::tungstenite::Message;

use crate::database::Database;
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
enum RequestPayload {
    Watch(StreamKind),
    Cancel,
    StepCursor(query::Cursor),
    Query2Cursor(query::Query),
    DoS(DebugDenialOfService),
}

#[derive(Serialize, Deserialize, Debug)]
enum ResponsePayload {
    Counters(crate::counters::Counters),
    Error(String),
}

#[derive(Serialize, Deserialize, Debug)]
enum StreamKind {
    Counters,
    Query(query::Query),
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
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
    async fn watch(&self, kind: &StreamKind, req_id: u64) {
        // {"id": 0, "payload": {"Watch": "Counters"}}
        // {"id": 0, "payload": "Cancel"}
        match kind {
            StreamKind::Counters => {
                let mut out_stream = self.stream_tx.clone();
                loop {
                    println!("counter tick");
                    out_stream.send(RespFrame {
                        id: req_id,
                        payload: ResponsePayload::Counters(crate::counters::Counters::default()),
                    }).await.unwrap();
                    tokio::time::delay_for(std::time::Duration::SECOND).await;
                }
            }
            StreamKind::Query(..) => todo!(),
        }
    }

    async fn dos(&self, kind: DebugDenialOfService) {
        // {"id": 0, "payload": {"DoS": "HoldWriteLock"}}
        // {"id": 1, "payload": {"DoS": "HoldWriteLock"}}
        // {"id": 0, "payload": "Cancel"}
        // {"id": 1, "payload": "Cancel"}
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

    async fn handle_req(self: Arc<Self>, req: ReqFrame, mut close_rx: broadcast::Receiver<()>) {
        // TODO(refactor)
        match &req.payload {
            RequestPayload::DoS(dos) => {
                futures::select! {
                    _ = self.dos(*dos).fuse() => {},
                    _ = close_rx.recv().fuse() => {
                        println!("req cancelled due to closed connection: {:?}", req)
                    }
                    _ = self.clone().await_cancel(req.id).fuse() => {
                        println!("req cancelled due to cancel message: {:?}", req)
                    }
                };
            }
            RequestPayload::Watch(kind) => {
                futures::select! {
                    _ = self.watch(kind, req.id).fuse() => {},
                    _ = close_rx.recv().fuse() => {
                        println!("req cancelled due to closed connection: {:?}", req)
                    }
                    _ = self.clone().await_cancel(req.id).fuse() => {
                        println!("req cancelled due to cancel message: {:?}", req)
                    }
                };
            }
            RequestPayload::Cancel => {
                let cancel_chans = self.cancel_chans.lock().await;
                if let Some(tx) = cancel_chans.get(&req.id).and_then(|tx| tx.upgrade()) {
                    let _ = tx.lock().await.take().unwrap().send(());
                }
            }
            _ => todo!(),
        };
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
                let msg = rmsg.expect("read.next().is_err()");
                match msg {
                    Message::Text(text) => {
                        let frame = serde_json::from_str::<ReqFrame>(&text);
                        if let Ok(frame) = frame {
                            dbg!(&frame);
                            write
                                .send(Message::Text(serde_json::to_string(&frame).unwrap()))
                                .await
                                .expect("failed to send");
                            tokio::spawn(conn_handler.clone().handle_req(
                                frame,
                                close_tx.subscribe(),
                            ));
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
