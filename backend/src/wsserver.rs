use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;

use futures::future::Future;
use futures::{SinkExt, StreamExt};
use tokio::net::TcpStream;
use tokio::sync::mpsc;
use tokio::sync::{broadcast, oneshot, Mutex};
use tokio_tungstenite::tungstenite::Message;

use crate::incr_counter;
use crate::stream::QueryIndex;
use crate::stream::{SegmentWithData, StreamDetails};
use crate::window::WindowHandle;
#[cfg(feature = "wirefilter")]
use crate::wirefilter::WirefilterContext;
use crate::{
    database::{Database, StreamID, TagID},
    throttled_watch::ThrottledWatch,
};

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
    GetTagID(crate::configuration::Tag),
    #[cfg(feature = "starlark")]
    StarlarkScan(StarlarkScanQuery),
    #[cfg(feature = "wirefilter")]
    WirefilterScan(StarlarkScanQuery),
    DoS(DebugDenialOfService),
    WindowUpdate {
        id: u64,
        params: crate::window::WindowParameters,
    },
    UpdateConfiguration(crate::configuration::ConfigurationUpdate),
    RegisterPipelineNode(crate::pipeline::WSNodeRegistration),
    PipelineResponse {
        stream_id: StreamID,
        response: crate::pipeline::NodeResponse,
    },
    ManagePipelineNode(ManagePipelineNode),
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) enum ResponsePayload {
    Counters(HashMap<String, u64>),
    Configuration(crate::configuration::Configuration),
    Error(String),
    WindowUpdate(crate::window::WindowUpdate),
    StreamDetails(StreamDetails),
    #[cfg(any(feature = "starlark", feature = "wirefilter"))]
    StarlarkScan(StarlarkScanResp),
    IndexSizes {
        services: HashMap<u16, u64>,
        tags: HashMap<TagID, u64>,
    },
    PipelineStatus(crate::pipeline::PipelineStatus),
    PipelineStream(crate::stream::StreamDetails),
    TagID(TagID),
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
enum ResponseStreamKind {
    Counters,
    Configuration,
    IndexSizes,
    PipelineStatus,
    Window {
        index: QueryIndex,
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
#[cfg(any(feature = "starlark", feature = "wirefilter"))]
struct StarlarkScanQuery {
    code: String,
    window_size: usize,
    bound_low: Option<StreamID>,
    bound_high: Option<StreamID>,
    reverse: bool,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
struct ScanResult {
    stream: crate::stream::LightweightStream,
    added_tags: SmallVec<[TagID; 4]>,
    attached: Option<serde_json::Value>,
    sort_key: Option<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
enum ManagePipelineNode {
    Disable(String),
    Enable(String),
    Remove(String),
    CatchUp(String),
    #[cfg(feature = "starlark")]
    AttachStarlark(String),
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
#[cfg(any(feature = "starlark", feature = "wirefilter"))]
pub(crate) struct StarlarkScanResp {
    error: Option<String>,
    scan_progress: StreamID, // inclusive
    range_exhausted: bool,
    bound_low: StreamID,
    bound_high: StreamID,
    scan_results: Vec<ScanResult>,
}

struct ConnectionHandler {
    db: Arc<Database>,
    cancel_chans: Mutex<HashMap<u64, oneshot::Sender<()>>>,
    windows: Mutex<HashMap<u64, Arc<WindowHandle>>>,
    pipeline_results_chan:
        Mutex<Option<broadcast::Sender<(StreamID, crate::pipeline::NodeResponse)>>>,
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
                let out_stream = self.stream_tx.clone();
                let mut watcher = crate::counters::subscribe();
                let mut prev = HashMap::new();
                loop {
                    let counters = watcher.borrow().clone();
                    let mut next = counters.as_hashmap();
                    next.retain(|k, v| v != prev.get(k).unwrap_or(&0));
                    let ws_tx_feedback_loop = next.len() == 1 && next.get("ws_tx").is_some();
                    if !ws_tx_feedback_loop {
                        out_stream
                            .send(RespFrame {
                                id: req_id,
                                payload: ResponsePayload::Counters(next),
                            })
                            .await
                            .unwrap();
                    }
                    prev = counters.as_hashmap();
                    watcher.changed().await.unwrap();
                }
            }
            ResponseStreamKind::Configuration => {
                let out_stream = self.stream_tx.clone();
                let mut handle = self.db.configuration_handle.clone();
                loop {
                    let config = handle.rx.borrow().clone();
                    out_stream
                        .send(RespFrame {
                            id: req_id,
                            payload: ResponsePayload::Configuration(config),
                        })
                        .await
                        .unwrap();
                    handle.rx.changed().await.unwrap();
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
                let tx = self.stream_tx.clone();

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
                            sender: segment.sender,
                        })
                    }
                    segments.reverse();
                    if segments.len() > 10_000 {
                        segments.truncate(10_000); // TODO big streams overwhelm the frontend
                    }
                    tx.send(RespFrame {
                        id: req_id,
                        payload: ResponsePayload::StreamDetails(StreamDetails {
                            id: stream.id,
                            client: stream.client,
                            server: stream.server,
                            // features: stream.features,
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
            ResponseStreamKind::IndexSizes => {
                let tx = self.stream_tx.clone();

                let db = self.db.clone();

                async fn fetch_index_sizes(db: &Database, req_id: u64) -> RespFrame {
                    let services = {
                        let services = db.services.read().await;
                        let mut count = HashMap::new();
                        let lots_of_services = services.len() > 256;
                        for (k, v) in services.iter() {
                            let v = v.read().await.streams.len();
                            if !lots_of_services || v > 100 {
                                // TODO: above mean?
                                count.insert(*k, v as u64);
                            }
                        }
                        count
                    };

                    let tags = {
                        let tag_index = db.tag_index.read().await;
                        let mut count = HashMap::new();
                        for (k, v) in tag_index.tagged.iter() {
                            count.insert(*k, v.len() as u64);
                        }
                        count
                    };

                    RespFrame {
                        id: req_id,
                        payload: ResponsePayload::IndexSizes { tags, services },
                    }
                }

                tx.send(fetch_index_sizes(&db, req_id).await).await.unwrap();
                let mut chan = ThrottledWatch::new(self.db.stream_notification_rx.clone());
                let mut update_tx = self.db.stream_update_tx.subscribe();
                loop {
                    tokio::select! {
                        _ = chan.next() => {
                            tx.send(fetch_index_sizes(&db, req_id).await).await.unwrap();
                        }
                        _ = update_tx.recv() => {
                            tx.send(fetch_index_sizes(&db, req_id).await).await.unwrap();
                        }
                    }
                }
            }
            ResponseStreamKind::PipelineStatus => {
                let tx = self.stream_tx.clone();
                let mut pipeline_topo_rx = {
                    let pipeline = self.db.pipeline.read().await;
                    pipeline.execution_plan_rx.clone()
                };
                let current_stream_id = *self.db.stream_notification_rx.borrow();
                {
                    let status = self
                        .db
                        .pipeline
                        .read()
                        .await
                        .status(current_stream_id)
                        .await;
                    tx.send(RespFrame {
                        id: req_id,
                        payload: ResponsePayload::PipelineStatus(status),
                    })
                    .await
                    .unwrap();
                }

                let mut chan = ThrottledWatch::new(self.db.stream_notification_rx.clone());

                loop {
                    tokio::select! {
                        _ = chan.next() => {
                            let current_stream_id = *self.db.stream_notification_rx.borrow();
                            let status = self.db.pipeline.read().await.status(current_stream_id).await;
                            tx.send(RespFrame {
                                id: req_id,
                                payload: ResponsePayload::PipelineStatus(status),
                            })
                            .await
                            .unwrap();
                        }
                        _ = pipeline_topo_rx.changed() => {
                            let current_stream_id = *self.db.stream_notification_rx.borrow();
                            let status = self.db.pipeline.read().await.status(current_stream_id).await;
                            tx.send(RespFrame {
                                id: req_id,
                                payload: ResponsePayload::PipelineStatus(status),
                            })
                            .await
                            .unwrap();
                        }
                    }
                }
            }
        }
    }

    async fn handle_pipeline_node(
        &self,
        req_id: u64,
        registration_info: &crate::pipeline::WSNodeRegistration,
    ) {
        let (submit_q, _node_guard) = {
            let (submit_q, results_chan, node_guard) =
                crate::pipeline::PipelineManager::register_ws(self.db.clone(), registration_info)
                    .await;
            *self.pipeline_results_chan.lock().await = Some(results_chan);
            (submit_q, node_guard)
        };

        let tx = self.stream_tx.clone();
        let mut buffer = Vec::new();
        loop {
            submit_q.pop_batch(&mut buffer).await;
            for swd in buffer.drain(..) {
                let stream_details = swd
                    .stream
                    .as_stream_details(&*swd.client_payload, &*swd.server_payload);
                tx.send(RespFrame {
                    id: req_id,
                    payload: ResponsePayload::PipelineStream(stream_details),
                })
                .await
                .unwrap();
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

    #[cfg(feature = "starlark")]
    async fn starlark_scan(
        &self,
        query: &StarlarkScanQuery,
    ) -> Result<ResponsePayload, ResponsePayload> {
        // tracyrs::zone!("starlark_scan");
        let config = self.db.configuration_handle.rx.borrow().clone();
        let latest_id = *self.db.stream_notification_rx.borrow();
        let bound_high = query.bound_high.unwrap_or(latest_id);
        let bound_low = query.bound_low.unwrap_or(StreamID::new(0));
        let scan_progress = if query.reverse { bound_low } else { bound_high };
        let range_exhausted = false;
        let streams = self.db.streams.read().await;

        let to_error_debug = |error: Box<dyn std::error::Error>| {
            ResponsePayload::StarlarkScan(StarlarkScanResp {
                bound_high,
                bound_low,
                scan_progress,
                range_exhausted,
                scan_results: Vec::new(),
                error: Some(format!("{:?}", error)),
            })
        };

        let filter_core =
            crate::scripting::StarlarkEngine::new(&query.code, config.clone(), self.db.clone());
        let index = filter_core.get_meta().map_err(to_error_debug)?;

        let mut scan_results = Vec::new();

        let scan_results_ref = &mut scan_results;

        const QUERY_SCAN_LIMIT: usize = 0x1000;
        let (range_exhausted, scan_progress) = self
            .db
            .with_index_iter(index, bound_low..=bound_high, move |iter| {
                let iter = if query.reverse {
                    Box::new(iter) as Box<dyn Iterator<Item = StreamID>>
                } else {
                    Box::new(iter.rev()) as Box<dyn Iterator<Item = StreamID>>
                };

                let mut range_exhausted = true;
                let mut scan_progress = scan_progress;
                for (i, stream_id) in iter.take(QUERY_SCAN_LIMIT).enumerate() {
                    if i == QUERY_SCAN_LIMIT - 1 {
                        range_exhausted = false;
                    }

                    let verdict = filter_core
                        .get_verdict(&streams[stream_id.idx()])
                        .map_err(to_error_debug)?;
                    if verdict.accept != Some(false) {
                        let stream = streams[stream_id.idx()].as_lightweight();
                        scan_results_ref.push(ScanResult {
                            stream,
                            added_tags: verdict.added_tags,
                            attached: verdict.attached,
                            sort_key: verdict.sort_key,
                        });
                        crate::incr_counter!(query_rows_returned);
                    }
                    scan_progress = stream_id;
                    crate::incr_counter!(query_rows_scanned);
                    if scan_results_ref.len() >= query.window_size {
                        range_exhausted = false;
                        break;
                    }
                }

                Ok((range_exhausted, scan_progress))
            })
            .await?;

        Ok(ResponsePayload::StarlarkScan(StarlarkScanResp {
            scan_progress,
            scan_results,
            range_exhausted,
            error: None,
            bound_high,
            bound_low,
        }))
    }

    #[cfg(feature = "wirefilter")]
    async fn wirefilter_scan(
        &self,
        query: &StarlarkScanQuery,
    ) -> Result<ResponsePayload, ResponsePayload> {
        let config = self.db.configuration_handle.rx.borrow().clone();
        let latest_id = *self.db.stream_notification_rx.borrow();
        let bound_high = query.bound_high.unwrap_or(latest_id);
        let bound_low = query.bound_low.unwrap_or(StreamID::new(0));
        let scan_progress = if query.reverse { bound_low } else { bound_high };
        let range_exhausted = false;
        let streams = self.db.streams.read().await;

        let scheme = WirefilterContext::make_scheme(&config);
        let mut ctx = WirefilterContext::new(&query.code, &scheme, config, self.db.clone())
            .map_err(|err| {
                ResponsePayload::StarlarkScan(StarlarkScanResp {
                    bound_high,
                    bound_low,
                    scan_progress,
                    range_exhausted,
                    scan_results: Vec::new(),
                    error: Some(format!("{}", err)),
                })
            })?;
        let index = ctx.get_index();

        let mut scan_results = Vec::new();

        let scan_results_ref = &mut scan_results;

        const QUERY_SCAN_LIMIT: usize = 0x1000;
        let (range_exhausted, scan_progress) = self
            .db
            .with_index_iter(index, bound_low..=bound_high, move |iter| {
                let iter = if query.reverse {
                    Box::new(iter) as Box<dyn Iterator<Item = StreamID>>
                } else {
                    Box::new(iter.rev()) as Box<dyn Iterator<Item = StreamID>>
                };

                let mut range_exhausted = true;
                let mut scan_progress = scan_progress;
                for (i, stream_id) in iter.take(QUERY_SCAN_LIMIT).enumerate() {
                    if i == QUERY_SCAN_LIMIT - 1 {
                        range_exhausted = false;
                    }

                    if ctx.matches(&streams[stream_id.idx()]) {
                        let stream = streams[stream_id.idx()].as_lightweight();
                        scan_results_ref.push(ScanResult {
                            stream,
                            added_tags: smallvec::smallvec![],
                            attached: None,
                            sort_key: None,
                        });
                        crate::incr_counter!(query_rows_returned);
                    }
                    scan_progress = stream_id;
                    crate::incr_counter!(query_rows_scanned);
                    if scan_results_ref.len() >= query.window_size {
                        range_exhausted = false;
                        break;
                    }
                }

                Ok((range_exhausted, scan_progress))
            })
            .await?;

        Ok(ResponsePayload::StarlarkScan(StarlarkScanResp {
            scan_progress,
            scan_results,
            range_exhausted,
            error: None,
            bound_high,
            bound_low,
        }))
    }

    async fn get_tag_id(&self, tag: &crate::configuration::Tag) -> ResponsePayload {
        let tag_id = self
            .db
            .configuration_handle
            .clone()
            .register_tag(&tag.slug, &tag.owner, &tag.name, &tag.color)
            .await;
        ResponsePayload::TagID(tag_id)
    }

    async fn manage_pipeline_node(&self, action: &ManagePipelineNode) {
        use crate::pipeline::NodeStatus;
        use ManagePipelineNode::*;
        let current_stream_id = *self.db.stream_notification_rx.borrow();
        let mut pipeline = self.db.pipeline.write().await;
        match action {
            Enable(node) => {
                let node = pipeline.get_node(node).unwrap();
                let mut node_status = node.status.lock().await;
                if *node_status == NodeStatus::Disabled {
                    *node_status = NodeStatus::Running;
                    node.missed_streams_tracker.end_range(current_stream_id);
                }
            }
            Disable(node) => {
                let node = pipeline.get_node(node).unwrap();
                let mut node_status = node.status.lock().await;
                if *node_status == NodeStatus::Running {
                    *node_status = NodeStatus::Disabled;
                    node.missed_streams_tracker.start_range(current_stream_id);
                }
            }
            Remove(_node) => todo!(),
            CatchUp(node) => {
                let node = pipeline.get_node(node).unwrap();
                let is_running = *node.status.lock().await == NodeStatus::Running;
                if is_running {
                    node.missed_streams_tracker.submit_to_node();
                }
            }
            #[cfg(feature = "starlark")]
            AttachStarlark(name) => {
                pipeline.start_starlark_tagger(self.db.clone(), name).await;
            }
        }
        pipeline.publish_topo();
    }

    async fn await_cancel(self: Arc<Self>, id: u64) {
        let (tx, rx) = tokio::sync::oneshot::channel();
        {
            self.cancel_chans.lock().await.insert(id, tx);
        }
        let _ = rx.await;
    }

    async fn setup_cancellation<T: Future>(self: Arc<Self>, req: &ReqFrame, f: T) {
        tokio::select! {
            _ = f => {},
            _ = self.clone().await_cancel(req.id) => {
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
        if let RequestPayload::Cancel = req.payload {
            let mut cancel_chans = self.cancel_chans.lock().await;
            let _ = cancel_chans.remove(&req.id); // drop cancel channel if present
            return;
        }

        if let RequestPayload::PipelineResponse {
            stream_id,
            response,
        } = req.payload
        {
            let results_chan = self.pipeline_results_chan.lock().await;
            results_chan
                .as_ref()
                .expect("pipeline response without registration?")
                .send((stream_id, response))
                .expect("failed to push result to PipelineNode");
            return;
        }

        // TODO(refactor)
        let self_ = self.clone();
        self.setup_cancellation(&req, async {
            match &req.payload {
                RequestPayload::DoS(dos) => self_.dos(dos).await,
                RequestPayload::Watch(kind) => self_.watch(req.id, kind).await,
                RequestPayload::WindowUpdate { id, params } => {
                    let windows = self_.windows.lock().await;
                    if let Some(window) = windows.get(id) {
                        window.update(params.clone()).await;
                    }
                }
                RequestPayload::UpdateConfiguration(conf_update) => {
                    let handle = self_.db.configuration_handle.clone();
                    handle.tx.send(conf_update.clone()).await.unwrap();
                }
                RequestPayload::RegisterPipelineNode(registration_info) => {
                    self_.handle_pipeline_node(req.id, registration_info).await;
                }
                RequestPayload::AddTag(stream_id, tag_id) => {
                    self_.db.add_tag(*stream_id, *tag_id).await
                }
                RequestPayload::RemoveTag(stream_id, tag_id) => {
                    self_.db.remove_tag(*stream_id, *tag_id).await
                }
                #[cfg(feature = "starlark")]
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
                #[cfg(feature = "wirefilter")]
                RequestPayload::WirefilterScan(query) => {
                    self_
                        .send_out(&req, async {
                            match self_.wirefilter_scan(query).await {
                                Ok(v) => v,
                                Err(v) => v,
                            }
                        })
                        .await
                }
                RequestPayload::GetTagID(tag) => self_.send_out(&req, self_.get_tag_id(tag)).await,
                RequestPayload::ManagePipelineNode(manage) => {
                    self_.manage_pipeline_node(manage).await
                }
                RequestPayload::Cancel => unreachable!(),
                RequestPayload::PipelineResponse { .. } => unreachable!(),
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

pub async fn accept_connection(stream: TcpStream, database: Arc<Database>) {
    let addr = stream
        .peer_addr()
        .expect("connected streams should have a peer address");
    println!("Peer address: {}", addr);

    let mut ws_stream = tokio_tungstenite::accept_async(stream)
        .await
        .expect("Error during the websocket handshake occurred");

    println!("New WebSocket connection: {}", addr);
    incr_counter!(ws_connections);

    let (stream_tx, mut stream_rx) = mpsc::channel::<RespFrame>(8);

    let conn_handler = Arc::new(ConnectionHandler {
        db: database,
        cancel_chans: Mutex::new(HashMap::new()),
        windows: Mutex::new(HashMap::new()),
        pipeline_results_chan: Mutex::new(None),
        stream_tx,
    });

    // let (mut write, mut read) = ws_stream.split();

    loop {
        tokio::select! {
            in_msg = ws_stream.next() => {
                let msg = match in_msg.unwrap() {
                    Ok(val) => val,
                    Err(e) => {
                        eprintln!("{:?}", e);
                        break;
                    }
                };
                incr_counter!(ws_rx);
                match msg {
                    Message::Text(text) => {
                        let frame = serde_json::from_str::<ReqFrame>(&text);
                        if let Ok(frame) = frame {
                            eprintln!("ws reqframe: {:?}", frame);
                            tokio::spawn(conn_handler.clone().handle_req(frame));
                        } else {
                            let resp = RespFrame {
                                id: 0x1337_1337_1337_1337,
                                payload: ResponsePayload::Error(format!("{:?}", frame)),
                            };
                            ws_stream
                                .send(Message::Text(serde_json::to_string(&resp).unwrap()))
                                .await
                                .expect("failed to send");
                        }
                    }
                    Message::Close(..) => {
                        let _ = ws_stream.send(msg).await;
                        break;
                    }
                    Message::Ping(_) => {} // handled by tungstenite
                    _ => panic!("unhandled msg frame {:?}", msg),
                }
            }
            out_msg = stream_rx.recv() => {
                incr_counter!(ws_tx);
                ws_stream
                    .send(Message::Text(serde_json::to_string(&out_msg.unwrap()).unwrap()))
                    .await
                    .expect("failed to send");
            }
        }
    }

    println!("WebSocket connection dropped: {}", addr);
    // NOTE: this aborts all in-flight requests and releases all held locks.
    conn_handler.cancel_chans.lock().await.drain();
}
