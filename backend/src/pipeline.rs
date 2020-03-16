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
            for action in self.process_stage(&self.reduce_stage, &swd).await {
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

    pub(crate) async fn register_node(&mut self, node: Arc<PipelineNode>) {
        self.execution_plan.add_node(node);
        self.execution_plan_tx
            .broadcast(self.execution_plan.clone())
            .unwrap();
    }

    pub(crate) async fn register_ws(
        db: Arc<crate::database::Database>,
        registration_info: &WSNodeRegistration,
    ) -> (
        Arc<WorkQ<StreamWithData>>,
        broadcast::Sender<(StreamID, NodeResponse)>,
        NodeGuard,
    ) {
        let (results_chan, _) = broadcast::channel(4);
        let submit_q = WorkQ::new(32, None);

        let node = Arc::new(crate::pipeline::PipelineNode::new(
            registration_info.name.clone(),
            registration_info.kind.clone(),
            submit_q.clone(),
            results_chan.clone(),
        ));
        db.pipeline.write().await.register_node(node.clone()).await;

        (submit_q, results_chan, NodeGuard(db, node))
    }

    pub(crate) async fn remove_node(&mut self, node: Arc<PipelineNode>, status: NodeStatus) {
        println!("removing pipeline node: {:?}", status);
        *node.status.lock().await = status;
        *node.results.lock().await = None;
        // TODO: remove from execution plan?
    }

    pub(crate) async fn status(&self) -> PipelineStatus {
        let mut nodes = HashMap::new();
        for mapper in &self.execution_plan.map_stage {
            nodes.insert(mapper.name.clone(), mapper.status_summary().await);
        }
        for mapper in &self.execution_plan.tag_stage {
            nodes.insert(mapper.name.clone(), mapper.status_summary().await);
        }
        for mapper in &self.execution_plan.reduce_stage {
            nodes.insert(mapper.name.clone(), mapper.status_summary().await);
        }
        PipelineStatus { nodes }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub(crate) enum NodeStatus {
    Running,         // processing
    Disabled,        // can be reenabled
    Errored(String), // permanently stopped
}

#[derive(Debug)]
pub(crate) struct PipelineNode {
    name: String,
    status: Mutex<NodeStatus>,
    kind: NodeKind,
    processed_streams: Mutex<u64>,
    output: Mutex<Option<Value>>,
    submit_q: Arc<WorkQ<StreamWithData>>,
    results: Mutex<Option<broadcast::Sender<(StreamID, NodeResponse)>>>,
}

impl PipelineNode {
    pub(crate) fn new(
        name: String,
        kind: NodeKind,
        submit_q: Arc<WorkQ<StreamWithData>>,
        results: broadcast::Sender<(StreamID, NodeResponse)>,
    ) -> PipelineNode {
        PipelineNode {
            name,
            kind,
            submit_q,
            results: Mutex::new(Some(results)),
            status: Mutex::new(NodeStatus::Running),
            output: Mutex::new(None),
            processed_streams: Mutex::new(0),
        }
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

    async fn status_summary(&self) -> NodeStatusSummary {
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
    output: Option<Value>,
    kind: NodeKind,
    status: NodeStatus,
    queued_streams: u64,
    processed_streams: u64,
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
