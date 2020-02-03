use crate::configuration::ConfigurationHandle;
use crate::incr_counter;
use crate::pipeline::PipelineManager;
use crate::query::QueryIndex;
use crate::reassembly::StreamReassembly;
use crate::stream::Stream;
use crate::workq::WorkQ;
use std::collections::{BTreeSet, BinaryHeap, HashMap};
use std::ops::RangeBounds;
use std::path::Path;
use std::sync::Arc;
use tokio::sync::{broadcast, watch, RwLock};

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

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct TagID(pub(crate) u16);
impl TagID {
    pub fn from_slug(slug: &[u8]) -> Self {
        use std::hash::Hasher;
        let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
        hasher.write(slug);
        TagID(hasher.finish() as u16)
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
    pub(crate) streams_queue: Arc<WorkQ<StreamReassembly>>,
}

impl Database {
    pub(crate) fn new(pcap_folder: &Path) -> Arc<Self> {
        tracyrs::zone!("Database::new");
        let payload_db = sled::Config::default()
            .cache_capacity(256 << 20) // 256 mb but memory usage grows a lot higher?
            .flush_every_ms(Some(10000))
            .use_compression(true)
            .compression_factor(3)
            .path("stream_payloads.sled")
            .open()
            .unwrap();
        let streams_queue = WorkQ::new(256, Some(b"StreamsWQ\0"));
        let (stream_notification_tx, stream_notification_rx) = watch::channel(StreamID(0));
        let (stream_update_tx, _) = broadcast::channel(128);
        let configuration_handle = crate::configuration::Configuration::spawn(pcap_folder);
        let db = Arc::new(Database {
            streams: RwLock::new(Vec::new()),
            tag_index: RwLock::new(TagIndex::new()),
            services: RwLock::new(HashMap::new()),
            pipeline: RwLock::new(PipelineManager::new()),
            configuration_handle,
            streams_queue: streams_queue.clone(),
            stream_notification_rx,
            stream_update_tx,
            payload_db,
        });
        tokio::spawn(
            db.clone()
                .ingest_streams_from(streams_queue, stream_notification_tx),
        );
        db
    }

    pub(crate) fn store_data(&self, data: &[u8]) -> StreamPayloadID {
        tracyrs::zone!("store_data");
        let id = {
            tracyrs::zone!("store_data", "hash");
            use std::hash::Hasher;
            let mut hasher = metrohash::MetroHash64::with_seed(0x1337_1337_1337_1337);
            hasher.write(data);
            StreamPayloadID(hasher.finish())
        };
        let key = id.0.to_be_bytes();
        if !self.payload_db.contains_key(key).unwrap() {
            tracyrs::zone!("store_data", "insert");
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

    pub(crate) async fn ingest_streams_from(
        self: Arc<Self>,
        rx: Arc<WorkQ<StreamReassembly>>,
        streamid_tx: watch::Sender<StreamID>,
    ) {
        // tracyrs::zone!("ingest_streams");
        let mut config_handle = self.configuration_handle.clone();
        let malformed_stream_tag_id = config_handle
            .register_tag(
                "malformed_stream",
                "reassembly",
                "Malformed Stream",
                "yellow",
            )
            .await;
        let cyclic_ack_id = config_handle
            .register_tag("cyclic_ack", "reassembly", "Cyclic Ack", "red")
            .await;
        let missing_data_id = config_handle
            .register_tag("missing_data", "reassembly", "Missing Data", "red")
            .await;
        let greedy_reassembly_id = config_handle
            .register_tag(
                "greedy_reassembly",
                "reassembly",
                "Greedy Reassembly",
                "red",
            )
            .await;

        let reconstruct_wq =
            WorkQ::<(StreamID, StreamReassembly)>::new(256, Some(b"ReconstructWQ\0"));
        let finished_streamid_wq = WorkQ::new(128, Some(b"SidWQ\0"));

        let workers = (num_cpus::get_physical() / 2).max(1);
        for _ in 0..workers {
            let reconstruct_wq = reconstruct_wq.clone();
            let finished_streamid_wq = finished_streamid_wq.clone();
            let db = self.clone();
            let pipeline_rx = self.pipeline.read().await.execution_plan_rx.clone();
            tokio::spawn(async move {
                let mut buffer = Vec::new();
                loop {
                    reconstruct_wq.pop_batch(&mut buffer).await;

                    for (stream_id, stream) in buffer.drain(..) {
                        let reconstructed = tokio::task::block_in_place(|| {
                            /*
                            if stream_id.idx() == 420 {
                                for _ in 0.. {
                                    tracyrs::zone!("DEBUG_BLOCK");
                                    std::thread::sleep(std::time::Duration::from_millis(420));
                                }
                            }
                            */
                            Stream::reconstruct_segments(stream)
                        });
                        let crate::stream::StreamSegmentResult {
                            client_data,
                            server_data,
                            malformed,
                            cyclic,
                            missing_data,
                            greedy_reassembly,
                            segments,
                        } = reconstructed;
                        let (client_data_id, server_data_id) = tokio::task::block_in_place(|| {
                            (db.store_data(&client_data), db.store_data(&server_data))
                        });

                        let service;
                        let tags;
                        let stream_copy;

                        {
                            let mut streams = db.streams.write().await;
                            let stream = &mut streams[stream_id.idx()];

                            if malformed {
                                stream.add_tag(malformed_stream_tag_id);
                            }
                            if cyclic {
                                stream.add_tag(cyclic_ack_id);
                            }
                            if missing_data {
                                stream.add_tag(missing_data_id);
                            }
                            if greedy_reassembly {
                                stream.add_tag(greedy_reassembly_id);
                            }

                            stream.segments = segments;
                            stream.client_data_id = client_data_id;
                            stream.server_data_id = server_data_id;
                            stream.client_data_len = client_data.len() as u32;
                            stream.server_data_len = server_data.len() as u32;

                            stream_copy = (*stream).clone();

                            service = stream.service();
                            tags = stream.tags.iter().cloned().collect::<Vec<TagID>>();
                        }
                        db.push_index(stream_id, service, &tags).await;

                        let stream_with_data = crate::stream::StreamWithData {
                            client_payload: Arc::new(client_data),
                            server_payload: Arc::new(server_data),
                            stream: Arc::new(stream_copy),
                        };

                        let execution_plan = { pipeline_rx.borrow().clone() }; // TODO: recv instead
                        execution_plan.process(stream_with_data).await;

                        finished_streamid_wq.push(stream_id).await;
                    }
                }
            });
        }

        tokio::spawn(async move {
            use std::cmp::Reverse;
            let mut next = 0;
            let mut pq = BinaryHeap::new();
            let mut buffer = Vec::new();
            loop {
                finished_streamid_wq.pop_batch(&mut buffer).await;
                for stream_id in buffer.drain(..) {
                    if stream_id.idx() == next {
                        streamid_tx.broadcast(stream_id).unwrap();
                        next = stream_id.idx() + 1;
                        continue;
                    }

                    pq.push(Reverse(stream_id.idx()));
                    crate::incr_counter!(streams_processed_out_of_order);

                    while pq.peek().copied() == Some(Reverse(next)) {
                        let Reverse(sid) = pq.pop().unwrap();
                        streamid_tx.broadcast(StreamID(sid)).unwrap();
                        next = sid + 1;
                    }
                }
            }
        });

        loop {
            let stream = rx.pop().await;
            let stream_id = {
                let mut streams = self.streams.write().await;
                tracyrs::zone!("streams push skeleton");
                let id = StreamID(streams.len());
                streams.push(Stream::skeleton_from(&stream, id));
                id
            };

            reconstruct_wq.push((stream_id, stream)).await;
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
            if !stream.add_tag(tag_id) {
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
            if !stream.remove_tag(tag_id) {
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
