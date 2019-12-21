use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};
use std::net::IpAddr;

use crate::incr_counter;

pub(crate) struct Stream {
    pub(crate) client: (IpAddr, u16),
    pub(crate) server: (IpAddr, u16),
    pub(crate) tags: Vec<TagID>,
    pub(crate) packets: Vec<(Sender, Vec<u8>)>,
}


pub(crate) enum Sender {
    Client,
    Server,
}

impl Stream {
    pub(crate) fn service(&self) -> u16 {
        return self.server.1;
    }
}

const SERVICE_PACKET_THRESHOLD: usize = 0x1000;

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) struct StreamID(usize);

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) struct TagID(u64);

#[derive(Default)]
pub(crate) struct TagIndex {
    pub(crate) tagged: HashMap<TagID, Vec<StreamID>>,
}

impl TagIndex {
    fn new() -> Self {
        TagIndex {
            tagged: HashMap::new(),
        }
    }
    fn push(&self, stream: &Stream) {
        todo!();
    }
}

pub(crate) struct Service {
    pub(crate) streams: Vec<StreamID>,
    pub(crate) tag_index: Option<TagIndex>,
}

impl Service {
    fn new() -> Self {
        Service {
            streams: Vec::new(),
            tag_index: None,
        }
    }
    fn push(&mut self, stream_id: StreamID, db: &Database) {
        self.streams.push(stream_id);
        if self.streams.len() == SERVICE_PACKET_THRESHOLD {
            incr_counter!(db_stat_service_promotion);
            let mut tag_index = TagIndex::default();
            let streams = db.streams.read().unwrap();
            for StreamID(idx) in &self.streams {
                tag_index.push(&streams[*idx]);
            }
            self.tag_index = Some(tag_index);
        }
    }
}

#[derive(Clone)]
pub(crate) struct Database {
    pub(crate) streams: Arc<RwLock<Vec<Stream>>>,
    pub(crate) tag_index: Arc<Mutex<TagIndex>>,
    pub(crate) services: Arc<RwLock<HashMap<u16, Arc<Mutex<Service>>>>>,
    pub(crate) counters: Arc<Mutex<crate::counters::Counters>>,
}

impl Database {
    pub(crate) fn new() -> Self {
        Database {
            streams: Arc::new(RwLock::new(Vec::new())),
            tag_index: Arc::new(Mutex::new(TagIndex::new())),
            services: Arc::new(RwLock::new(HashMap::new())),
            counters: Arc::new(Mutex::new(crate::counters::Counters::default())),
        }
    }

    pub(crate) fn push(&self, stream: Stream) {
        // TODO: hold writer lock while parsing whole pcap?
        assert!(stream.tags.is_empty());
        let service = stream.service();
        let stream_id = {
            let mut streams = self.streams.write().unwrap();
            let id = streams.len();
            streams.push(stream);
            StreamID(id)
        };

        self.services
            .write()
            .unwrap()
            .entry(service)
            .or_insert_with(|| {
                incr_counter!(db_services);
                Arc::new(Mutex::new(Service::new()))
            })
            .lock()
            .unwrap()
            .push(stream_id, &self);
    }
}
