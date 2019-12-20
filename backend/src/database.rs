use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};

pub(crate) struct StreamRef {
    uuid: u64,
}

#[derive(Hash, PartialEq, Eq)]
pub(crate) struct TagID(u64);

pub(crate) struct Counters {
    packets: u64,
    streams: u64,
    reassembly_errors: u64,
}

pub(crate) struct TagIndex {
    pub(crate) tagged: HashMap<TagID, StreamRef>,
}

pub(crate) struct Service {
    pub(crate) stream: Vec<StreamRef>,
}

pub(crate) struct Database {
    pub(crate) streams: Arc<Mutex<Vec<StreamRef>>>,
    pub(crate) services: Arc<RwLock<HashMap<u16, Arc<Mutex<Service>>>>>,
    pub(crate) counters: Arc<Mutex<Counters>>,
}

impl Database {
    pub(crate) fn start() -> Self {
        unimplemented!()
    }
}
