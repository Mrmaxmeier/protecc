use crate::configuration::ConfigurationHandle;
use crate::incr_counter;
use crate::pipeline::PipelineManager;
use crate::query::QueryIndex;
use crate::reassembly::StreamReassembly;
use crate::stream::Stream;
use std::collections::{BTreeSet, HashMap};
use std::ops::RangeBounds;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{broadcast, mpsc, watch, RwLock};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub(crate) enum Sender {
    Client,
    Server,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Segment {
    pub(crate) sender: Sender,
    pub(crate) start: usize,
    pub(crate) timestamp: u64,
    pub(crate) flags: u8,
    pub(crate) seq: u32,
    pub(crate) ack: u32,
}

impl Stream {
    pub(crate) fn service(&self) -> u16 {
        self.server.1
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct StreamID(usize);
impl StreamID {
    pub(crate) fn new(idx: usize) -> Self {
        StreamID(idx)
    }
    pub(crate) fn idx(&self) -> usize {
        self.0
    }
    pub(crate) fn next(&self) -> StreamID {
        StreamID(self.0 + 1)
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) struct StreamPayloadID(pub u64);

impl Serialize for StreamPayloadID {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        format!("{}", self.0).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for StreamPayloadID {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(String::deserialize(deserializer)?
            .parse::<u64>()
            .map(StreamPayloadID)
            .unwrap())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct TagID(pub(crate) u32);
impl TagID {
    pub fn from_slug(slug: &[u8]) -> Self {
        use std::hash::Hasher;
        let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
        hasher.write(slug);
        TagID(hasher.finish() as u32)
    }
}

#[derive(Default)]
pub(crate) struct TagIndex {
    pub(crate) tagged: HashMap<TagID, BTreeSet<StreamID>>,
}

impl TagIndex {
    fn new() -> Self {
        TagIndex {
            tagged: HashMap::new(),
        }
    }
    fn push(&mut self, stream_id: StreamID, tags: &[TagID]) {
        for tag in tags {
            self.tagged.entry(*tag).or_default().insert(stream_id);
        }
    }
}

pub(crate) struct Service {
    pub(crate) streams: BTreeSet<StreamID>,
    pub(crate) tag_index: TagIndex,
}

impl Service {
    fn new() -> Self {
        Service {
            streams: BTreeSet::new(),
            tag_index: TagIndex::default(),
        }
    }
    fn push(&mut self, stream_id: StreamID) {
        self.streams.insert(stream_id);
    }
}

pub(crate) struct Database {
    pub(crate) streams: RwLock<Vec<Stream>>,
    pub(crate) stream_notification_rx: watch::Receiver<StreamID>,
    pub(crate) stream_update_tx: broadcast::Sender<StreamID>,
    pub(crate) tag_index: RwLock<TagIndex>,
    pub(crate) services: RwLock<HashMap<u16, Arc<RwLock<Service>>>>,
    pub(crate) pipeline: RwLock<PipelineManager>,
    pub(crate) configuration_handle: ConfigurationHandle,
    pub(crate) payload_db: sled::Db,
    pub(crate) ingest_tx: mpsc::Sender<StreamReassembly>,
}

impl Database {
    pub(crate) fn new() -> Arc<Self> {
        tracyrs::zone!("Database::new");
        let payload_db = sled::Config::default()
            .cache_capacity(64 << 20) // 64 mb but memory usage grows a lot higher?
            .use_compression(true)
            .compression_factor(3)
            .path("stream_payloads.sled")
            .open()
            .unwrap();
        let (ingest_tx, ingest_rx) = mpsc::channel(256);
        let (stream_notification_tx, stream_notification_rx) = watch::channel(StreamID(0));
        let (stream_update_tx, _) = broadcast::channel(128);
        let configuration_handle = crate::configuration::Configuration::spawn();
        let db = Arc::new(Database {
            streams: RwLock::new(Vec::new()),
            tag_index: RwLock::new(TagIndex::new()),
            services: RwLock::new(HashMap::new()),
            pipeline: RwLock::new(PipelineManager::new()),
            configuration_handle,
            ingest_tx,
            stream_notification_rx,
            stream_update_tx,
            payload_db,
        });
        tokio::spawn(
            db.clone()
                .ingest_streams_from(ingest_rx, stream_notification_tx),
        );
        db
    }

    pub(crate) fn store_data(&self, data: &[u8]) -> StreamPayloadID {
        tracyrs::zone!("Database::store_data");
        use std::hash::Hasher;
        let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
        hasher.write(data);
        let id = StreamPayloadID(hasher.finish());
        let key = id.0.to_be_bytes();
        if !self.payload_db.contains_key(key).unwrap() {
            self.payload_db
                .insert(key, data)
                .expect("Failed to write to Sled");
        }
        id
    }

    pub(crate) fn datablob(&self, id: StreamPayloadID) -> Option<sled::IVec> {
        // TODO(perf): read into pooled buffer
        self.payload_db
            .get(id.0.to_be_bytes())
            .expect("failed to read from Sled")
    }

    // should only be called with stream_id == self.streams.len()-1
    async fn push_index(&self, stream_id: StreamID, service: u16, tags: &[TagID]) {
        let mut services = self.services.write().await;
        let service_index = services.entry(service).or_insert_with(|| {
            incr_counter!(db_services);
            Arc::new(RwLock::new(Service::new()))
        });
        service_index.write().await.push(stream_id);
        if !tags.is_empty() {
            let mut ti = self.tag_index.write().await;
            ti.push(stream_id, tags);
            service_index.write().await.tag_index.push(stream_id, tags);
        }
    }

    pub(crate) async fn push(&self, mut stream: Stream) -> StreamID {
        let service = stream.service();
        let tags = stream.tags.iter().cloned().collect::<Vec<TagID>>();
        let stream_id = {
            let mut streams = self.streams.write().await;
            let id = StreamID(streams.len());
            stream.id = id;
            streams.push(stream);
            id
        };

        crate::counters::update_counters(|c| {
            c.db_streams_rss += std::mem::size_of::<Stream>() as u64
        });

        self.push_index(stream_id, service, &tags).await;

        stream_id
    }

    pub(crate) async fn ingest_streams_from(
        self: Arc<Self>,
        mut rx: mpsc::Receiver<StreamReassembly>,
        streamid_tx: watch::Sender<StreamID>,
    ) {
        tracyrs::zone!("ingest_streams");
        let malformed_stream_tag_id = self
            .configuration_handle
            .clone()
            .register_tag(
                "malformed_stream",
                "reassembly",
                "Malformed Stream",
                "yellow",
            )
            .await;
        let cyclic_ack_id = self
            .configuration_handle
            .clone()
            .register_tag("cyclic_ack", "reassembly", "Cyclic Ack", "red")
            .await;
        let missing_data_id = self
            .configuration_handle
            .clone()
            .register_tag("missing_data", "reassembly", "Missing Data", "red")
            .await;
        while let Some(stream) = rx.next().await {
            tracyrs::zone!("ingest_streams", "ingesting stream");
            let (mut stream, is_malformed, is_cyclic) = Stream::from(stream, &self).await;
            if is_malformed {
                stream.tags.insert(malformed_stream_tag_id);
            }
            if is_cyclic {
                stream.tags.insert(cyclic_ack_id);
            }
            // TODO: pipeline
            let stream_id = self.push(stream).await;
            streamid_tx.broadcast(stream_id).unwrap();
        }
    }

    pub(crate) async fn modify_tag_indices<F: Fn(&mut BTreeSet<StreamID>)>(
        &self,
        tag_id: TagID,
        service: u16,
        f: F,
    ) {
        {
            let mut index = self.tag_index.write().await;
            f(index.tagged.entry(tag_id).or_default())
        }

        {
            let services = self.services.read().await;
            if let Some(service) = services.get(&service) {
                let mut service = service.write().await;
                f(service.tag_index.tagged.entry(tag_id).or_default())
            }
        }
    }

    pub(crate) async fn add_tag(&self, stream_id: StreamID, tag_id: TagID) {
        let service = {
            let stream = &mut self.streams.write().await[stream_id.idx()];
            if !stream.tags.insert(tag_id) {
                return;
            }
            stream.server.1
        };
        self.modify_tag_indices(tag_id, service, |idx| {
            idx.insert(stream_id);
        })
        .await;
        self.stream_update_tx.send(stream_id).unwrap(); // TODO: send (stream_id, tag_id)?
    }

    pub(crate) async fn remove_tag(&self, stream_id: StreamID, tag_id: TagID) {
        let service = {
            let stream = &mut self.streams.write().await[stream_id.idx()];
            if !stream.tags.remove(&tag_id) {
                return;
            }
            stream.server.1
        };
        self.modify_tag_indices(tag_id, service, |idx| {
            idx.remove(&stream_id);
        })
        .await;
        self.stream_update_tx.send(stream_id).unwrap();
    }

    pub(crate) async fn with_index_iter<R, F, O>(&self, index: QueryIndex, r: R, f: F) -> O
    where
        F: FnOnce(&mut dyn DoubleEndedIterator<Item = StreamID>) -> O,
        R: RangeBounds<StreamID>,
    {
        /* an API like this would be pretty nice :/
        /// Note: The relevant index is locked for the lifetime of the returned value
        pub(crate) async fn index_iter_range<'a, R: RangeBounds<StreamID>, F, O>(&'a self, index: QueryIndex, r: R) -> TagIndexIterator<'a>
        */
        match index {
            QueryIndex::All => {
                return match r.end_bound() {
                    std::ops::Bound::Included(end) => f(&mut (0..=end.idx()).map(StreamID)),
                    std::ops::Bound::Excluded(end) => f(&mut (0..end.idx()).map(StreamID)),
                    std::ops::Bound::Unbounded => unreachable!(),
                }
            }
            QueryIndex::Service(port) => {
                let services = self.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    return f(&mut service.streams.range(r).copied());
                }
            }
            QueryIndex::ServiceTagged(port, tag) => {
                let services = self.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(streams) = service.tag_index.tagged.get(&tag) {
                        return f(&mut streams.range(r).copied());
                    }
                }
            }
            QueryIndex::Tagged(tag) => {
                let tag_index = self.tag_index.read().await;
                if let Some(streams) = tag_index.tagged.get(&tag) {
                    return f(&mut streams.range(r).copied());
                }
            }
        }
        f(&mut std::iter::empty())
    }
}
