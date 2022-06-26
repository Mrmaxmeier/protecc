use crate::database::{Database, StreamID, TagID};
use crate::stream::{SegmentWithData, Stream, StreamWithData};
use crate::workq::WorkQ;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{broadcast, watch, Mutex};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub(crate) enum NodeResponse {
    Neutral, // assumed for all streams if connection to node dies
    ReplacePayloads {
        segments: Vec<SegmentWithData>,
        tags: SmallVec<[TagID; 4]>,
    },
    TagWith(SmallVec<[TagID; 4]>),
}

// TODO: support multiple nodes per nodeid
#[derive(Clone, Default, Debug)]
pub(crate) struct ExecutionPlan {
    map_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    tag_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    reduce_stage: Vec<Arc<PipelineNode>>, // wait for these acks
}

impl ExecutionPlan {
    async fn process_stage(
        &self,
        stage: &[Arc<PipelineNode>],
        swd: &StreamWithData,
    ) -> SmallVec<[NodeResponse; 4]> {
        let mut response_futures = Vec::with_capacity(stage.len());
        for node in stage {
            if *node.status.lock().await != NodeStatus::Running {
                continue;
            }
            /*
            if node
                .filter
                .matches(&mut StreamDataWrapper::StreamWithData(stream), db)
            {
            */
            response_futures.push(node.submit(swd.clone()));
            //}
        }
        // TODO: FuturesUnordered?
        let mut res = SmallVec::new();
        for action in &futures::future::join_all(response_futures).await {
            if let Some(action) = action {
                res.push(action.clone());
            }
        }
        res
    }

    pub(crate) async fn process(&self, swd: StreamWithData, db: &Database) {
        let mut stream_tags = swd.stream.tags.clone();

        if !self.map_stage.is_empty() {
            let mut changed = false;
            for action in self.process_stage(&self.map_stage, &swd).await {
                match action {
                    NodeResponse::Neutral => {}
                    NodeResponse::ReplacePayloads { segments, tags } => {
                        if changed {
                            debug_assert!(false, "single stream by multiple mappers");
                        }
                        changed = true;
                        for tag in &tags {
                            if !stream_tags.contains(tag) {
                                db.tag_index.write().await.push(swd.stream.id, &[*tag]);
                                stream_tags.push(*tag);
                            }
                        }
                        let (client_data, server_data, segments) = SegmentWithData::pack(segments);
                        let stream = &mut db.streams.write().await[swd.stream.id.idx()];
                        stream.client_data_len = client_data.len() as u32;
                        stream.client_data_id = db.store_data(&client_data);
                        stream.server_data_len = server_data.len() as u32;
                        stream.server_data_id = db.store_data(&server_data);
                        stream.segments = segments;
                    }
                    _ => debug_assert!(false, "mapper sent invalid response? {:?}", action),
                }
            }
        }

        if !self.tag_stage.is_empty() {
            for action in self.process_stage(&self.tag_stage, &swd).await {
                match action {
                    NodeResponse::Neutral => {}
                    NodeResponse::TagWith(tags) => {
                        for tag in &tags {
                            if !stream_tags.contains(tag) {
                                db.tag_index.write().await.push(swd.stream.id, &[*tag]);
                                stream_tags.push(*tag);
                            }
                        }
                    }
                    _ => debug_assert!(false, "tagger sent invalid response? {:?}", action),
                }
            }
        }

        if stream_tags.len() != swd.stream.tags.len() {
            db.streams.write().await[swd.stream.id.idx()].tags = stream_tags;
        }
        if !self.reduce_stage.is_empty() {
            for _action in self.process_stage(&self.reduce_stage, &swd).await {
                todo!();
            }
        }
    }

    fn add_node(&mut self, node: Arc<PipelineNode>) {
        match node.kind {
            NodeKind::Mapper => self.map_stage.push(node),
            NodeKind::Tagger => self.tag_stage.push(node),
            NodeKind::Reducer => self.reduce_stage.push(node),
        }
    }
}

pub(crate) struct PipelineManager {
    execution_plan: ExecutionPlan,
    pub(crate) execution_plan_rx: watch::Receiver<ExecutionPlan>,
    execution_plan_tx: watch::Sender<ExecutionPlan>,
}

impl PipelineManager {
    pub(crate) fn new() -> Self {
        let (execution_plan_tx, execution_plan_rx) = watch::channel(ExecutionPlan::default());
        PipelineManager {
            execution_plan: ExecutionPlan::default(),
            execution_plan_rx,
            execution_plan_tx,
        }
    }

    pub(crate) fn register_node(&mut self, node: Arc<PipelineNode>) {
        self.execution_plan.add_node(node);
        self.publish_topo();
    }

    pub(crate) async fn register_ws(
        db: Arc<crate::database::Database>,
        registration_info: &WSNodeRegistration,
    ) -> (
        Arc<WorkQ<StreamWithData>>,
        broadcast::Sender<(StreamID, NodeResponse)>,
        NodeGuard,
    ) {
        let (results_chan, _) = broadcast::channel(16);
        let submit_q = WorkQ::new(32, None);

        let node = crate::pipeline::PipelineNode::new(
            registration_info.name.clone(),
            registration_info.kind.clone(),
            db.clone(),
            submit_q.clone(),
            results_chan.clone(),
            false,
        );
        db.pipeline.write().await.register_node(node.clone());

        (submit_q, results_chan, NodeGuard(db, node))
    }

    #[cfg(feature = "starlark")]
    pub(crate) async fn start_starlark_tagger(
        &mut self,
        db: Arc<crate::database::Database>,
        name: &str,
    ) {
        println!("start_starlark_tagger");
        let (results_chan, _) = broadcast::channel(16);
        let submit_q = WorkQ::new(32, None);

        let node = crate::pipeline::PipelineNode::new(
            name.into(),
            NodeKind::Tagger,
            db.clone(),
            submit_q.clone(),
            results_chan.clone(),
            true,
        );
        self.register_node(node.clone());

        let config = db.configuration_handle.rx.borrow().clone();
        let script = config.scripts[name].to_owned();
        let core = crate::scripting::StarlarkEngine::new(&script, config, db).unwrap();

        tokio::spawn(async move {
            let mut buffer = Vec::new();
            let error;
            'main: loop {
                submit_q.pop_batch(&mut buffer).await;
                for swd in buffer.drain(..) {
                    // TODO: got swd but only using .stream
                    let verdict = core.get_verdict(&*swd.stream);
                    let res = match verdict {
                        Ok(res) => res,
                        Err(err) => {
                            error = format!("{:?}", err);
                            break 'main;
                        }
                    };
                    let resp = if res.added_tags.is_empty() {
                        NodeResponse::Neutral
                    } else {
                        NodeResponse::TagWith(res.added_tags)
                    };
                    results_chan.send((swd.stream.id, resp)).unwrap();
                }
            }

            let mut status = node.status.lock().await;
            *status = NodeStatus::Errored(error);
        });
    }

    pub(crate) fn get_node(&mut self, node: &str) -> Option<Arc<PipelineNode>> {
        for mapper in &self.execution_plan.map_stage {
            if mapper.name == node {
                return Some(mapper.clone());
            }
        }
        for tagger in &self.execution_plan.tag_stage {
            if tagger.name == node {
                return Some(tagger.clone());
            }
        }
        for reducer in &self.execution_plan.reduce_stage {
            if reducer.name == node {
                return Some(reducer.clone());
            }
        }
        None
    }

    pub(crate) async fn remove_node(&mut self, node: Arc<PipelineNode>, status: NodeStatus) {
        println!("removing pipeline node: {:?}", status);
        *node.status.lock().await = status;
        *node.results.lock().await = None;
        // TODO: remove from execution plan?
    }

    pub(crate) async fn status(&self, current_stream_id: StreamID) -> PipelineStatus {
        let mut nodes = HashMap::new();
        for mapper in &self.execution_plan.map_stage {
            nodes.insert(
                mapper.name.clone(),
                mapper.status_summary(current_stream_id).await,
            );
        }
        for mapper in &self.execution_plan.tag_stage {
            nodes.insert(
                mapper.name.clone(),
                mapper.status_summary(current_stream_id).await,
            );
        }
        for mapper in &self.execution_plan.reduce_stage {
            nodes.insert(
                mapper.name.clone(),
                mapper.status_summary(current_stream_id).await,
            );
        }
        PipelineStatus { nodes }
    }

    pub(crate) fn publish_topo(&self) {
        self.execution_plan_tx
            .send(self.execution_plan.clone())
            .unwrap();
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub(crate) enum NodeStatus {
    Running,         // processing
    Disabled,        // can be reenabled
    Errored(String), // permanently stopped
}

#[derive(Debug, Clone)]
enum MissedStreamsPacket {
    StartRange(StreamID),
    EndRange(StreamID),
    SubmitToNode,
    #[allow(unused)]
    Exit,
}

#[derive(Debug, Clone)]
struct MissedStreamsStatus {
    missed: usize,
    missing_since: Option<StreamID>,
    active: bool,
}

#[derive(Debug)]
// TODO: this design is flawed in several ways. catch-up only works if in 'running' state and streamid are not synchronized
pub(crate) struct MissedStreamsTrackerHandle {
    tx: broadcast::Sender<MissedStreamsPacket>,
    rx: watch::Receiver<MissedStreamsStatus>,
}

impl MissedStreamsTrackerHandle {
    pub(crate) fn new(
        db: Arc<Database>,
        node: tokio::sync::oneshot::Receiver<Arc<PipelineNode>>,
    ) -> Self {
        let (packet_tx, packet_rx) = broadcast::channel(8);
        let (status_tx, status_rx) = watch::channel(MissedStreamsStatus {
            missed: 0,
            missing_since: None,
            active: false,
        });
        let current_id = *db.stream_notification_rx.borrow();

        tokio::spawn(async move {
            let node = node.await.unwrap();
            Self::handle(db, node, packet_rx, status_tx).await;
        });
        packet_tx
            .send(MissedStreamsPacket::StartRange(StreamID::new(0)))
            .unwrap();
        packet_tx
            .send(MissedStreamsPacket::EndRange(current_id))
            .unwrap();
        MissedStreamsTrackerHandle {
            tx: packet_tx,
            rx: status_rx,
        }
    }

    async fn handle(
        db: Arc<Database>,
        node_q: Arc<PipelineNode>,
        mut packet_rx: broadcast::Receiver<MissedStreamsPacket>,
        status_tx: watch::Sender<MissedStreamsStatus>,
    ) {
        let mut packets = Vec::new();
        while let Ok(packet) = packet_rx.recv().await {
            match packet {
                MissedStreamsPacket::StartRange(..) | MissedStreamsPacket::EndRange(..) => {
                    packets.push(packet);
                }
                MissedStreamsPacket::SubmitToNode => {
                    packets.push(packet);
                    status_tx.send(Self::calc_missed(&packets)).unwrap();
                    while Self::advance(&mut packets, &*db, &node_q).await {
                        status_tx.send(Self::calc_missed(&packets)).unwrap();
                    }
                }
                MissedStreamsPacket::Exit => break,
            }
            status_tx.send(Self::calc_missed(&packets)).unwrap();
        }
    }

    fn calc_missed(packets: &[MissedStreamsPacket]) -> MissedStreamsStatus {
        let mut missed = 0;
        let mut active = false;
        let mut missing_since = None;
        for packet in packets {
            match packet {
                MissedStreamsPacket::StartRange(id) => {
                    assert!(missing_since.is_none());
                    missing_since = Some(*id);
                }
                MissedStreamsPacket::EndRange(id) => {
                    let since = missing_since.take().unwrap();
                    missed += id.idx() - since.idx();
                }
                MissedStreamsPacket::SubmitToNode => {
                    active = true;
                }
                _ => unreachable!(),
            };
        }
        MissedStreamsStatus {
            missed,
            active,
            missing_since,
        }
    }

    async fn advance(
        packets: &mut Vec<MissedStreamsPacket>,
        db: &Database,
        node: &PipelineNode,
    ) -> bool {
        if packets.len() < 2 {
            packets.clear();
            return false;
        }
        if let (
            MissedStreamsPacket::StartRange(range_start),
            MissedStreamsPacket::EndRange(mut range_end),
        ) = (packets[0].clone(), packets[1].clone())
        {
            if range_end.idx() - range_start.idx() > 200 {
                // defer rest to next iteratino so that we can keep sending status updates
                packets[0] =
                    MissedStreamsPacket::StartRange(StreamID::new(range_start.idx() + 100));
                range_end = StreamID::new(range_start.idx() + 100);
            } else {
                packets.remove(0);
                packets.remove(0);
            }

            for id in range_start.idx()..range_end.idx() {
                let stream = db.streams.read().await[id].clone();
                let stream_id = stream.id;
                let client_payload = db.datablob(stream.client_data_id).unwrap().to_vec();
                let server_payload = db.datablob(stream.server_data_id).unwrap().to_vec();
                let swd = StreamWithData {
                    stream: Arc::new(stream),
                    client_payload: Arc::new(client_payload),
                    server_payload: Arc::new(server_payload),
                };
                if let Some(resp) = node.submit(swd).await {
                    match resp {
                        NodeResponse::Neutral => {}
                        NodeResponse::TagWith(tags) => {
                            for tag in tags {
                                db.add_tag(stream_id, tag).await;
                            }
                        }
                        NodeResponse::ReplacePayloads { .. } => unimplemented!(),
                    }
                };
            }
        } else {
            unreachable!();
        }
        true
    }

    pub(crate) fn start_range(&self, id: StreamID) {
        self.tx.send(MissedStreamsPacket::StartRange(id)).unwrap();
    }
    pub(crate) fn end_range(&self, id: StreamID) {
        self.tx.send(MissedStreamsPacket::EndRange(id)).unwrap();
    }
    pub(crate) fn submit_to_node(&self) {
        self.tx.send(MissedStreamsPacket::SubmitToNode).unwrap();
    }
    #[allow(unused)]
    pub(crate) fn exit(&self) {
        self.tx.send(MissedStreamsPacket::Exit).unwrap();
    }
    pub(crate) fn status(&self, current_id: StreamID) -> (u64, bool) {
        let status = self.rx.borrow().clone();
        let mut missing = status.missed;
        if let Some(stream_id) = status.missing_since {
            missing += current_id.idx() - stream_id.idx();
        }
        (missing as u64, status.active)
    }
}

#[derive(Debug)]
pub(crate) struct PipelineNode {
    pub(crate) name: String,
    pub(crate) status: Mutex<NodeStatus>,
    pub(crate) kind: NodeKind,
    pub(crate) processed_streams: Mutex<u64>,
    pub(crate) output: Mutex<Option<Value>>,
    pub(crate) submit_q: Arc<WorkQ<StreamWithData>>,
    pub(crate) results: Mutex<Option<broadcast::Sender<(StreamID, NodeResponse)>>>,
    pub(crate) missed_streams_tracker: MissedStreamsTrackerHandle,
    pub(crate) is_starlark: bool,
}

impl PipelineNode {
    pub(crate) fn new(
        name: String,
        kind: NodeKind,
        db: Arc<Database>,
        submit_q: Arc<WorkQ<StreamWithData>>,
        results: broadcast::Sender<(StreamID, NodeResponse)>,
        is_starlark: bool,
    ) -> Arc<PipelineNode> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let node = PipelineNode {
            missed_streams_tracker: MissedStreamsTrackerHandle::new(db, rx),
            name,
            kind,
            submit_q,
            results: Mutex::new(Some(results)),
            status: Mutex::new(NodeStatus::Running),
            output: Mutex::new(None),
            processed_streams: Mutex::new(0),
            is_starlark,
        };
        let arc = Arc::new(node);
        tx.send(arc.clone()).unwrap();
        arc
    }

    async fn submit(&self, swd: StreamWithData) -> Option<NodeResponse> {
        let id = swd.stream.id;
        let mut rx = {
            match self.results.lock().await.as_ref() {
                Some(results_chan) => results_chan.subscribe(),
                None => return None,
            }
        };
        self.submit_q.push(swd).await;
        loop {
            let (stream_id, result) = match rx.recv().await {
                Ok(v) => v,
                Err(_) => {
                    self.submit_q.try_drain().await; // TODO: is this needed?
                    return None;
                }
            };
            if stream_id == id {
                *self.processed_streams.lock().await += 1;
                return Some(result);
            }
        }
    }

    async fn status_summary(&self, current_stream_id: StreamID) -> NodeStatusSummary {
        let (missed_streams, catching_up) = self.missed_streams_tracker.status(current_stream_id);
        NodeStatusSummary {
            kind: self.kind.clone(),
            status: self.status.lock().await.clone(),
            name: self.name.clone(),
            output: self.output.lock().await.clone(),
            processed_streams: *self.processed_streams.lock().await,
            queued_streams: self
                .results
                .lock()
                .await
                .as_ref()
                .map(|chan| chan.receiver_count() as u64)
                .unwrap_or(0),
            missed_streams,
            catching_up,
            is_starlark: self.is_starlark,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub(crate) enum NodeKind {
    Mapper,
    Reducer,
    Tagger, // can also add feature vectors?
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct WSNodeRegistration {
    name: String,
    kind: NodeKind,
    metadata_only: bool,
    filter: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct NewStreamNotification {
    stream_id: StreamID,
    stream: Stream,
    client_stream_payload: Option<Vec<u8>>,
    server_stream_payload: Option<Vec<u8>>,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct NodeStatusSummary {
    name: String,
    is_starlark: bool,
    output: Option<Value>,
    kind: NodeKind,
    status: NodeStatus,
    queued_streams: u64,
    processed_streams: u64,
    missed_streams: u64,
    catching_up: bool,
}
#[derive(Serialize, Debug)]
pub(crate) struct PipelineStatus {
    nodes: HashMap<String, NodeStatusSummary>,
}

pub(crate) struct NodeGuard(Arc<crate::database::Database>, Arc<PipelineNode>);

impl Drop for NodeGuard {
    fn drop(&mut self) {
        let node = self.1.clone();
        let db = self.0.clone();
        tokio::task::spawn(async move {
            let mut pipeline_manager = db.pipeline.write().await;
            pipeline_manager
                .remove_node(
                    node,
                    NodeStatus::Errored("websocket connection lost".into()),
                )
                .await;
        });
    }
}
