use serde_json::Value;

struct PipelineManager {
    nodes: Vec<PipelineNode>,
}

struct PipelineNode {
    name: String,
    last_acked: u64,
    kind: PipelineKind,
    state: Option<Value>,
}

pub(crate) enum PipelineKind {
    Mapper,
    Reducer,
    Tagger, // also adds features
}

struct PipelineRegistration {
    name: String,
    kind: PipelineKind,
    filter: crate::query::QueryFilter,
}