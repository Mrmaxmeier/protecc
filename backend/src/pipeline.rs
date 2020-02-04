use crate::database::{StreamID, TagID};
use crate::stream::{Stream, StreamWithData};
use crate::workq::WorkQ;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{broadcast, watch, Mutex};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) enum NodeResponse {
    Neutral, // assumed for all streams if connection to node dies
    ReplacePayloads {
        new_segments: Vec<crate::stream::SegmentWithData>,
    }, // TODO
    TagWith(SmallVec<[TagID; 4]>),
}

// TODO: support multiple nodes per nodeid
#[derive(Clone, Default)]
pub(crate) struct ExecutionPlan {
    map_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    tag_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    reduce_stage: Vec<Arc<PipelineNode>>, // wait for these acks
}

impl ExecutionPlan {
    pub(crate) async fn process(&self, swd: StreamWithData) {
        if !self.map_stage.is_empty() {
            let mut map_results = Vec::with_capacity(self.map_stage.len());
            for node in &self.map_stage {
                /*
                if node
                    .filter
                    .matches(&mut StreamDataWrapper::StreamWithData(stream), db)
                {
                */
                map_results.push(node.submit(swd.clone()));
                //}
            }
            // TODO: FuturesUnordered?
            let mut changed = false;
            for action in &futures::future::join_all(map_results).await {
                if let NodeResponse::ReplacePayloads { new_segments } = action {
                    if changed {
                        debug_assert!(false, "single stream by multiple mappers");
                    }
                    changed = true;
                    panic!("TODO: mapper {:?}", (new_segments, changed));
                } else if let NodeResponse::Neutral = action {
                } else {
                    debug_assert!(false, "mapper sent invalid response? {:?}", action);
                }
            }
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

    pub(crate) async fn register_node(&mut self, node: Arc<PipelineNode>) {}

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
        db.pipeline.write().await.register_node(node.clone());

        (submit_q, results_chan, NodeGuard(db, node))
    }

    pub(crate) async fn remove_node(&mut self, node: Arc<PipelineNode>) {
        todo!()
    }

    pub(crate) async fn status(&self) -> PipelineStatus {
        let mut nodes = HashMap::new();
        for mapper in &self.execution_plan.map_stage {
            nodes.insert(mapper.name.clone(), mapper.status_summary().await);
        }
        PipelineStatus { nodes }
    }
}

#[derive(Serialize, Debug, Clone)]
enum NodeStatus {
    Running,         // processing
    Disabled,        // can be reenabled
    Errored(String), // permanently stopped
}

pub(crate) struct PipelineNode {
    name: String,
    status: Mutex<NodeStatus>,
    kind: NodeKind,
    output: Mutex<Option<Value>>,
    submit_q: Arc<WorkQ<StreamWithData>>,
    results: broadcast::Sender<(StreamID, NodeResponse)>,
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
            results,
            status: Mutex::new(NodeStatus::Running),
            output: Mutex::new(None),
        }
    }

    async fn submit(&self, swd: StreamWithData) -> NodeResponse {
        let id = swd.stream.id;
        let mut rx = self.results.subscribe();
        self.submit_q.push(swd).await;
        loop {
            let (stream_id, result) = rx
                .recv()
                .await
                .expect("pipeline node results chan dropped?"); // TODO: handle pipeline node remove
            if stream_id == id {
                return result;
            }
        }
    }

    async fn status_summary(&self) -> NodeStatusSummary {
        NodeStatusSummary {
            kind: self.kind.clone(),
            status: self.status.lock().await.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
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
pub(crate) struct NodeStatusSummary {
    kind: NodeKind,
    status: NodeStatus,
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
            pipeline_manager.remove_node(node).await;
        });
    }
}
