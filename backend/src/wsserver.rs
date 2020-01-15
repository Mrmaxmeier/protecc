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

use crate::database::{Database, StreamID, TagID};
use crate::incr_counter;
use crate::query;
use crate::stream::{SegmentWithData, Stream, StreamDetails};
use crate::window::WindowHandle;

#[derive(Serialize, Debug)]
pub(crate) struct RespFrame {
    pub(crate) id: u64,
    pub(crate) payload: ResponsePayload,
}

#[derive(Deserialize, Debug)]
struct ReqFrame {
    id: u64,
    payload: RequestPayload,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum RequestPayload {
    Watch(ResponseStreamKind),
    Cancel,
    AddTag(StreamID, TagID),
    RemoveTag(StreamID, TagID),
    StepCursor(query::Cursor),
    Query2Cursor(query::Query),
    StarlarkScan(StarlarkScanQuery),
    DoS(DebugDenialOfService),
    WindowUpdate {
        id: u64,
        params: crate::window::WindowParameters,
    },
    RegisterActor(crate::pipeline::PipelineRegistration),
    UpdateConfiguration(crate::configuration::ConfigurationUpdate),
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ResponsePayload {
    Counters(HashMap<String, u64>),
    Configuration(crate::configuration::Configuration),
    Cursor(query::Cursor),
    CursorResult(query::Cursor, Vec<Stream>, bool),
    Error(String),
    WindowUpdate(crate::window::WindowUpdate),
    StreamDetails(StreamDetails),
    StarlarkScan(StarlarkScanResp),
}

#[derive(Deserialize, Debug)]
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

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct StarlarkScanQuery {
    code: String,
    window_size: usize,
    bound_low: Option<StreamID>,
    bound_high: Option<StreamID>,
    reverse: bool,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StarlarkScanResp {
    error: Option<String>,
    scan_progress: StreamID, // inclusive
    range_exhausted: bool,
    bound_low: StreamID,
    bound_high: StreamID,
    scan_results: Vec<crate::stream::LightweightStream>, // TODO: attached data
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
        // {"id": 0, "payload": {"watch": "configuration"}}
        match kind {
            ResponseStreamKind::Counters => {
                let mut out_stream = self.stream_tx.clone();
                let mut watcher = crate::counters::subscribe();
                let mut prev = HashMap::new();
                while let Some(counters) = watcher.recv().await {
                    let mut next = counters.as_hashmap();
                    next.retain(|k, v| v != prev.get(k).unwrap_or(&0));
                    if next.len() == 1 && next.get("ws_tx").is_some() {
                        continue;
                    }
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
                let mut update_id_chan = self.db.stream_update_tx.subscribe();

                loop {
                    let stream = {
                        let streams = db.streams.read().await;
                        streams
                            .get(stream_id.idx())
                            .expect("stream details for unknown stream requested")
                            .clone()
                    };
                    let client_payload_ = db
                        .datablob(stream.client_data_id)
                        .expect("couldn't find client payload");
                    let mut client_payload = client_payload_.as_ref();
                    let server_payload_ = db
                        .datablob(stream.server_data_id)
                        .expect("couldn't find server payload");
                    let mut server_payload = server_payload_.as_ref();
                    let mut segments = Vec::new();
                    for segment in stream.segments.iter().rev() {
                        use crate::database::Sender;
                        let data = match segment.sender {
                            Sender::Client => {
                                let (a, b) = client_payload.split_at(segment.start);
                                client_payload = a;
                                b
                            }
                            Sender::Server => {
                                let (a, b) = server_payload.split_at(segment.start);
                                server_payload = a;
                                b
                            }
                        };
                        segments.push(SegmentWithData {
                            data: data.to_vec(),
                            seq: segment.seq,
                            ack: segment.ack,
                            flags: segment.flags,
                            timestamp: segment.timestamp,
                            sender: segment.sender.clone(),
                        })
                    }
                    segments.reverse();
                    tx.send(RespFrame {
                        id: req_id,
                        payload: ResponsePayload::StreamDetails(StreamDetails {
                            id: stream.id,
                            client: stream.client,
                            server: stream.server,
                            features: stream.features,
                            tags: stream.tags,
                            client_data_len: stream.client_data_len,
                            server_data_len: stream.server_data_len,
                            segments,
                        }),
                    })
                    .await
                    .unwrap();

                    loop {
                        let id = update_id_chan.recv().await.unwrap();
                        if id == stream.id {
                            break;
                        }
                    }
                }
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

    async fn starlark_scan(
        &self,
        query: &StarlarkScanQuery,
    ) -> Result<ResponsePayload, ResponsePayload> {
        let config = self
            .db
            .configuration_handle
            .clone()
            .rx
            .recv()
            .await
            .unwrap();
        let latest_id = self.db.stream_notification_rx.clone().recv().await.unwrap();
        let bound_high = query.bound_high.unwrap_or(latest_id);
        let bound_low = query.bound_low.unwrap_or(StreamID::new(0));
        let scan_progress = if query.reverse { bound_low } else { bound_high };
        let range_exhausted = false;
        let streams = self.db.streams.read().await;

        // This makes me sad.
        // QueryFilterCore is !Send and can't cross await bounds
        // => collect all streamids that'll be searched and
        //    construct filter_core twice.
        let mut toscan_streamids = Vec::new();

        let diag_to_error = |diagnostic: starlark::codemap_diagnostic::Diagnostic| {
            ResponsePayload::StarlarkScan(StarlarkScanResp {
                bound_high,
                bound_low,
                scan_progress,
                range_exhausted,
                scan_results: Vec::new(),
                error: Some(format!("{:?}", diagnostic)),
            })
        };

        let exception_to_error = |diagnostic: starlark::eval::EvalException| {
            ResponsePayload::StarlarkScan(StarlarkScanResp {
                bound_high,
                bound_low,
                scan_progress,
                range_exhausted,
                scan_results: Vec::new(),
                error: Some(format!("{:?}", diagnostic)),
            })
        };

        let index = {
            let filter_core =
                crate::starlark::QueryFilterCore::new(&query.code, config.clone(), self.db.clone())
                    .map_err(diag_to_error)?;
            filter_core.get_meta().map_err(exception_to_error)?
        };
        self.db
            .with_index_iter(index, bound_low..bound_high, |iter| {
                toscan_streamids = iter.rev().take(0x1000).collect();
            })
            .await;

        if query.reverse {
            toscan_streamids.reverse();
        }

        let mut scan_results = Vec::new();
        let mut scan_progress = scan_progress;
        let mut range_exhausted = toscan_streamids.len() < 0x1000;
        {
            let filter_core =
                crate::starlark::QueryFilterCore::new(&query.code, config, self.db.clone())
                    .map_err(diag_to_error)?;
            for stream_id in toscan_streamids {
                if filter_core
                    .get_verdict(&streams[stream_id.idx()])
                    .map_err(exception_to_error)?
                    .accept
                    == Some(true)
                {
                    scan_results.push(streams[stream_id.idx()].as_lightweight());
                }
                scan_progress = stream_id;
                if scan_results.len() >= query.window_size {
                    range_exhausted = false;
                    break;
                }
            }
        }
        Ok(ResponsePayload::StarlarkScan(StarlarkScanResp {
            scan_progress,
            scan_results,
            range_exhausted,
            error: None,
            bound_high,
            bound_low,
        }))
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
                RequestPayload::UpdateConfiguration(conf_update) => {
                    let mut handle = self_.db.configuration_handle.clone();
                    handle.tx.send(conf_update.clone()).await.unwrap();
                }
                RequestPayload::RegisterActor(..) => todo!(),
                RequestPayload::AddTag(stream_id, tag_id) => {
                    self_.db.add_tag(*stream_id, *tag_id).await
                }
                RequestPayload::RemoveTag(stream_id, tag_id) => {
                    self_.db.remove_tag(*stream_id, *tag_id).await
                }
                RequestPayload::StarlarkScan(query) => {
                    self_
                        .send_out(&req, async {
                            match self_.starlark_scan(query).await {
                                Ok(v) => v,
                                Err(v) => v,
                            }
                        })
                        .await
                }
                RequestPayload::Cancel => unreachable!(),
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
