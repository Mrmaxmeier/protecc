use serde::{Deserialize, Serialize};
use serde_json::Value;
use crate::database::{Stream, StreamID};
use tokio::sync::{watch, RwLock};
use std::sync::Arc;

#[derive(Debug, Clone)]
struct NodeStatus {
    streamid_pending: Option<StreamID>,
    streamid_ack: Option<StreamID>,
    state: Option<Value>,
}

#[derive(Debug, Clone)]
struct ExecutionPlan {
    map_stage: Vec<Arc<PipelineNode>>, // wait for these acks
    tag_stage: Vec<Arc<PipelineNode>>, // wait for these acks
    reduce_stage: Vec<Arc<PipelineNode>>, // wait for these acks
}

pub(crate) struct PipelineManager {
    nodes: Vec<Arc<PipelineNode>>,
    execution_plan: Vec<Vec<usize>>,
    last_streamid: Option<StreamID>,
    execution_plan_rx: watch::Receiver<ExecutionPlan>,
    selfdestruct_rx: watch::Receiver<bool>,
}

impl PipelineManager {
    pub(crate) fn new() -> Self {
        let (selfdestruct_tx, selfdestruct_rx) = watch::channel(false);
        let (execution_plan_tx, execution_plan_rx) = watch::channel(ExecutionPlan {
            stages: Vec::new()
        });
        PipelineManager {
            nodes: Vec::new(),
            execution_plan: Vec::new(),
            last_streamid: None,
            execution_plan_rx,
            selfdestruct_rx,
        }
    }
    fn recalc(&mut self) {
        self.execution_plan.clear();
        self.nodes.sort_by_key(|node| match node.kind {
            PipelineKind::Mapper => 0,
            PipelineKind::Tagger => 1,
            PipelineKind::Reducer => 2,
        });
    }
}

#[derive(Debug)]
struct PipelineNode {
    name: String,
    last_acked: u64,
    kind: PipelineKind,
    state: Option<NodeStatus>,
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) enum PipelineKind {
    Mapper,
    Reducer,
    Tagger, // can also add features vectors?
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct PipelineRegistration {
    name: String,
    kind: PipelineKind,
    ignore_payloads: bool,
    filter: crate::query::QueryFilter,
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct NewStreamNotification {
    stream_id: StreamID,
    stream: Stream,
    client_stream_payload: Option<Vec<u8>>,
    server_stream_payload: Option<Vec<u8>>,
}