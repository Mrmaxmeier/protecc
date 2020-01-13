#![allow(unused, dead_code, unused_variables)] // TODO
use crate::database::{Database, StreamID, TagID};
use crate::stream::{Stream, StreamDataWrapper, StreamWithData};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::watch;

enum PipelineResponse {
    Neutral, // assumed for all packets iff connection to node dies
    TagWith(TagID),
    TagWithMultiple(Vec<TagID>),
}

#[derive(Debug, Clone)]
struct NodeStatus {
    streamid_pending: Option<StreamID>,
    streamid_ack: Option<StreamID>,
    state: Option<Value>,
}

#[derive(Debug, Clone, Default)]
struct ExecutionPlan {
    map_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    tag_stage: Vec<Arc<PipelineNode>>,    // wait for these acks
    reduce_stage: Vec<Arc<PipelineNode>>, // wait for these acks
}

impl ExecutionPlan {
    pub(crate) async fn submit<'a>(&self, stream: StreamWithData<'a>, db: &'a Database) {
        /*
        // db is unused actually
        if !self.map_stage.is_empty() {
            let map_results = Vec::new();
            for node in &self.map_stage {
                if node
                    .filter
                    .matches(&mut StreamDataWrapper::StreamWithData(stream), db)
                {
                    node.submit(stream).await;
                    map_results.push(node.await_resp(stream));
                }
            }
            // TODO: FuturesUnordered
            for res in futures::future::join_all(map_results) {}
        }
        */
    }
}

pub(crate) struct PipelineManager {
    nodes: Vec<Arc<PipelineNode>>,
    execution_plan: ExecutionPlan,
    last_streamid: Option<StreamID>,
    execution_plan_rx: watch::Receiver<ExecutionPlan>,
    selfdestruct_rx: watch::Receiver<bool>,
}

impl PipelineManager {
    pub(crate) fn new() -> Self {
        let (selfdestruct_tx, selfdestruct_rx) = watch::channel(false);
        let (execution_plan_tx, execution_plan_rx) = watch::channel(ExecutionPlan::default());
        PipelineManager {
            nodes: Vec::new(),
            execution_plan: ExecutionPlan::default(),
            last_streamid: None,
            execution_plan_rx,
            selfdestruct_rx,
        }
    }
}

#[derive(Debug)]
struct PipelineNode {
    name: String,
    last_acked: u64,
    kind: PipelineKind,
    state: Option<NodeStatus>,
    filter: crate::query::QueryFilter,
}

impl PipelineNode {
    pub(crate) async fn submit<'a>(self: Arc<Self>, stream: StreamWithData<'a>) {}
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
    ignore_payloads: Option<bool>,
    filter: crate::query::QueryFilter,
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct NewStreamNotification {
    stream_id: StreamID,
    stream: Stream,
    client_stream_payload: Option<Vec<u8>>,
    server_stream_payload: Option<Vec<u8>>,
}
