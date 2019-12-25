use std::collections::{HashMap, HashSet};
use std::net::IpAddr;
use std::sync::{Arc, Mutex, RwLock};

use crate::incr_counter;

use serde::{Deserialize, Serialize};
use sled;

/*
TODO(perf/footprint):
- replace Vec with smallvec
- replace hashset with same-alloc set (tinyset)
- replace (sender, usize) with u32 & 0x7fffffff
- replace hashmap with smallvec?

MAYBE(footprint):
- global cache of ipaddrs
*/
// Note: Stream should be small and cheap to clone.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Stream {
    pub(crate) client: (IpAddr, u16),
    pub(crate) server: (IpAddr, u16),
    pub(crate) tags: HashSet<TagID>,
    pub(crate) features: HashMap<TagID, f64>,
    pub(crate) segments: Vec<(Sender, usize)>,
    pub(crate) client_data_id: u64,
    pub(crate) server_data_id: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct StreamID(usize);
impl StreamID {
    pub(crate) fn idx(&self) -> usize { self.0 }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
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

pub(crate) struct Database {
    pub(crate) streams: RwLock<Vec<Stream>>,
    pub(crate) tag_index: Mutex<TagIndex>,
    pub(crate) services: RwLock<HashMap<u16, Arc<Mutex<Service>>>>,
    pub(crate) counters: Mutex<crate::counters::Counters>,
    pub(crate) sled_db: sled::Db,
}

impl Database {
    pub(crate) fn new() -> Self {
        let sled_db = sled::Db::open("/tmp/snacc_payload_database").unwrap();
        if !sled_db.is_empty() {
            println!("[WARN] clearing previous sled db");
            sled_db.clear().unwrap();
        }
        Database {
            streams: RwLock::new(Vec::new()),
            tag_index: Mutex::new(TagIndex::new()),
            services: RwLock::new(HashMap::new()),
            counters: Mutex::new(crate::counters::Counters::default()),
            sled_db,
        }
    }

    pub(crate) fn push_raw(
        &self,
        client: (IpAddr, u16),
        server: (IpAddr, u16),
        segments: Vec<(Sender, usize)>,
        client_data: Vec<u8>,
        server_data: Vec<u8>,
    ) {
        let client_data_id = self.sled_db.generate_id().unwrap();
        let server_data_id = self.sled_db.generate_id().unwrap();
        self.sled_db
            .insert(client_data_id.to_be_bytes(), client_data)
            .unwrap();
        self.sled_db
            .insert(server_data_id.to_be_bytes(), server_data)
            .unwrap();
        let stream = Stream {
            client,
            server,
            segments,
            client_data_id,
            server_data_id,
            tags: HashSet::new(),
            features: HashMap::new(),
        };
        self.push(stream);
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
