use serde::{Deserialize, Serialize};
use std::sync::Arc;

use futures::sink::SinkExt;
use futures::StreamExt;
use futures::future::FutureExt;
use tokio::net::TcpStream;
use tokio::sync::broadcast;
use tokio_tungstenite::tungstenite::Message;

use crate::database::Database;

#[derive(Serialize, Deserialize, Debug)]
struct Frame {
    request_id: u64,
    payload: FramePayload,
}

#[derive(Serialize, Deserialize, Debug)]
enum FramePayload {
    SubscribeCounters,
    CounterState(crate::counters::Counters),
    Query2Cursor,
    DoS(DebugDenialOfService),
    Error(String),
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
    addr: std::net::SocketAddr,
}

impl ConnectionHandler {
    async fn dos(&self, kind: DebugDenialOfService) {
        // {"request_id": 0, "payload": {"DoS": "HoldWriteLock"}}
        use DebugDenialOfService::*;
        match kind {
            HoldReadLock => {
                println!("[DEBUG] thread is acquiring read lock");
                let _guard = self.db.streams.read().await;
                println!("[DEBUG] thread is holding read lock");
                let _guard = self.db.streams.write().await; // deadlock
            },
            HoldWriteLock => {
                println!("[DEBUG] thread is acquiring write lock");
                let _guard = self.db.streams.write().await;
                println!("[DEBUG] thread is holding write lock");
                let _guard = self.db.streams.read().await; // deadlock
            },
            Panic => panic!("{:?}", kind),
            HoldAndPanic => {
                let _guard = self.db.streams.read().await;
                panic!("{:?}", kind);
            },
        }
    }

    async fn handle_req(self: Arc<Self>, req: Frame, mut close_rx: broadcast::Receiver<()>) {
        match &req.payload {
            FramePayload::DoS(dos) => {
                futures::select! {
                    _ = self.dos(*dos).fuse() => {},
                    _ = close_rx.recv().fuse() => {
                        println!("dropping req due to closed connection: {:?}", req)
                    }
                };
            },
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
    let conn_handler = Arc::new(ConnectionHandler { db: database, addr });

    let (close_tx, _) = broadcast::channel(1);

    let (mut write, mut read) = ws_stream.split();
    while let Some(msg) = read.next().await {
        let msg = msg.unwrap();
        match msg {
            Message::Text(text) => {
                let frame = serde_json::from_str::<Frame>(&text);
                if let Ok(frame) = frame {
                    dbg!(&frame);
                    write
                        .send(Message::Text(serde_json::to_string(&frame).unwrap()))
                        .await
                        .expect("failed to send");
                    tokio::spawn(conn_handler.clone().handle_req(frame, close_tx.subscribe()));
                } else {
                    let resp = Frame {
                        request_id: 0,
                        payload: FramePayload::Error(format!("{:?}", frame)),
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

    println!("WebSocket connection dropped: {}", addr);
    drop(write); // NOTE: this canceles all waiting / lock-bearing in-flight requests
}
