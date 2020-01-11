use std::collections::HashMap;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch, RwLock};

use crate::configuration::ConfigurationHandle;
use crate::incr_counter;
use crate::pipeline::PipelineManager;
use crate::reassembly::StreamReassembly;
pub(crate) use crate::stream::Stream;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Debug, Clone, Serialize, Deserialize)]
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
}

impl Stream {
    pub(crate) fn service(&self) -> u16 {
        self.server.1
    }
}

const SERVICE_PACKET_THRESHOLD: usize = 0x400;

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
pub(crate) struct TagID(pub(crate) u64);

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
        if !stream.tags.is_empty() {
            todo!();
        }
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
    async fn push(&mut self, stream_id: StreamID, db: &Database) {
        self.streams.push(stream_id);
        if self.streams.len() == SERVICE_PACKET_THRESHOLD {
            incr_counter!(db_stat_service_promotion);
            let tag_index = TagIndex::default();
            let streams = db.streams.read().await;
            for StreamID(idx) in &self.streams {
                tag_index.push(&streams[*idx]);
            }
            self.tag_index = Some(tag_index);
        }
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

    pub(crate) async fn push(&self, mut stream: Stream) -> StreamID {
        // TODO: refactor
        // TODO(perf?): hold writer lock while parsing whole pcap?
        assert!(stream.tags.is_empty());
        let service = stream.service();
        let stream_id = {
            let mut streams = self.streams.write().await;
            let id = StreamID(streams.len());
            stream.id = id;
            streams.push(stream);
            id
        };

        self.services
            .write()
            .await
            .entry(service)
            .or_insert_with(|| {
                incr_counter!(db_services);
                Arc::new(RwLock::new(Service::new()))
            })
            .write()
            .await
            .push(stream_id, &self)
            .await;

        stream_id
    }

    pub(crate) async fn ingest_streams_from(
        self: Arc<Self>,
        mut rx: mpsc::Receiver<StreamReassembly>,
        streamid_tx: watch::Sender<StreamID>,
    ) {
        tracyrs::zone!("ingest_streams");
        while let Some(stream) = rx.next().await {
            tracyrs::zone!("ingest_streams", "ingesting stream");
            let stream = Stream::from(stream, &self).await;
            let stream_id = self.push(stream).await;
            streamid_tx.broadcast(stream_id).unwrap();
        }
    }
}
