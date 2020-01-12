use crate::configuration::ConfigurationHandle;
use crate::incr_counter;
use crate::pipeline::PipelineManager;
use crate::reassembly::StreamReassembly;
use crate::stream::Stream;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch, RwLock};

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

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub(crate) struct StreamID(usize);
impl StreamID {
    pub(crate) fn new(idx: usize) -> Self {
        StreamID(idx)
    }
    pub(crate) fn idx(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) struct StreamPayloadID(u64);

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
pub(crate) struct TagID(u32);
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
    pub(crate) tagged: HashMap<TagID, Vec<StreamID>>, // TODO(footprint) smallvec optimization
}

impl TagIndex {
    fn new() -> Self {
        TagIndex {
            tagged: HashMap::new(),
        }
    }
    fn push(&mut self, stream_id: StreamID, tags: &[TagID]) {
        for tag in tags {
            self.tagged.entry(*tag).or_default().push(stream_id);
        }
    }
}

pub(crate) struct Service {
    pub(crate) streams: Vec<StreamID>,
    pub(crate) tag_index: TagIndex,
}

impl Service {
    fn new() -> Self {
        Service {
            streams: Vec::new(),
            tag_index: TagIndex::default(),
        }
    }
    fn push(&mut self, stream_id: StreamID) {
        self.streams.push(stream_id);
    }
}

pub(crate) struct Database {
    pub(crate) streams: RwLock<Vec<Stream>>,
    pub(crate) stream_notification_rx: watch::Receiver<StreamID>,
    pub(crate) tag_index: RwLock<TagIndex>,
    pub(crate) services: RwLock<HashMap<u16, Arc<RwLock<Service>>>>,
    pub(crate) pipeline: RwLock<PipelineManager>,
    pub(crate) configuration_handle: ConfigurationHandle,
    pub(crate) payload_db: rocksdb::DB,
    pub(crate) ingest_tx: mpsc::Sender<StreamReassembly>,
}

impl Database {
    pub(crate) fn new() -> Arc<Self> {
        tracyrs::zone!("Database::new");
        let mut opts = rocksdb::Options::default();
        opts.set_write_buffer_size(256 << 20);
        opts.set_compression_type(rocksdb::DBCompressionType::Zstd);
        opts.set_max_background_compactions(4);
        opts.set_max_background_flushes(2);
        opts.create_if_missing(true);
        let payload_db = rocksdb::DB::open(&opts, "stream_payloads.rocksdb").unwrap();
        let (ingest_tx, ingest_rx) = mpsc::channel(256);
        let (stream_notification_tx, stream_notification_rx) = watch::channel(StreamID(0));
        let configuration_handle = crate::configuration::Configuration::spawn();
        let db = Arc::new(Database {
            streams: RwLock::new(Vec::new()),
            tag_index: RwLock::new(TagIndex::new()),
            services: RwLock::new(HashMap::new()),
            pipeline: RwLock::new(PipelineManager::new()),
            configuration_handle,
            ingest_tx,
            stream_notification_rx,
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
        self.payload_db
            .put(id.0.to_be_bytes(), data)
            .expect("Failed to write to RocksDB");
        id
    }

    pub(crate) fn datablob(&self, id: StreamPayloadID) -> Option<Vec<u8>> {
        // TODO(perf): read into pooled buffer
        self.payload_db
            .get(id.0.to_be_bytes())
            .expect("failed to read from RocksDB")
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
            .register_tag("malformed-stream".into(), "reassembly".into())
            .await;
        while let Some(stream) = rx.next().await {
            tracyrs::zone!("ingest_streams", "ingesting stream");
            let is_malformed = stream.malformed;
            let mut stream = Stream::from(stream, &self).await;
            if is_malformed {
                stream.tags.insert(malformed_stream_tag_id);
            }
            // TODO: pipeline
            let stream_id = self.push(stream).await;
            streamid_tx.broadcast(stream_id).unwrap();
        }
    }
}
