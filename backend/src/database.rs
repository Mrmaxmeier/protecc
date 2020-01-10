use std::collections::{HashMap, HashSet};
use std::net::IpAddr;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch, RwLock};

use crate::incr_counter;
use crate::pipeline::PipelineManager;
use crate::reassembly::StreamReassembly;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
#[serde(rename_all = "camelCase")]
pub(crate) struct Stream {
    pub(crate) id: StreamID,
    pub(crate) client: (IpAddr, u16),
    pub(crate) server: (IpAddr, u16),
    pub(crate) tags: HashSet<TagID>,
    pub(crate) features: HashMap<TagID, f64>,
    pub(crate) segments: Vec<Segment>,
    pub(crate) client_data_len: u32,
    pub(crate) client_data_id: StreamPayloadID,
    pub(crate) server_data_len: u32,
    pub(crate) server_data_id: StreamPayloadID,
}

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
        let db = Arc::new(Database {
            streams: RwLock::new(Vec::new()),
            tag_index: RwLock::new(TagIndex::new()),
            services: RwLock::new(HashMap::new()),
            pipeline: RwLock::new(PipelineManager::new()),
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

    fn store_data(&self, data: &[u8]) -> StreamPayloadID {
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

    pub(crate) async fn push_raw(
        &self,
        client: (IpAddr, u16),
        server: (IpAddr, u16),
        segments: Vec<Segment>,
        client_data: &[u8],
        server_data: &[u8],
        streamid_tx: &mut watch::Sender<StreamID>, // TODO refactor
    ) {
        tracyrs::zone!("Database::push_raw");
        let client_data_id = self.store_data(client_data);
        let server_data_id = self.store_data(server_data);
        let stream = Stream {
            id: StreamID(0),
            client,
            server,
            segments,
            client_data_len: client_data.len() as u32,
            server_data_len: server_data.len() as u32,
            client_data_id,
            server_data_id,
            tags: HashSet::new(),
            features: HashMap::new(),
        };
        self.push(stream, streamid_tx).await;
    }

    pub(crate) async fn push(&self, mut stream: Stream, streamid_tx: &mut watch::Sender<StreamID>) {
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

        streamid_tx.broadcast(stream_id).unwrap();

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
    }

    pub(crate) async fn ingest_streams_from(
        self: Arc<Self>,
        mut rx: mpsc::Receiver<StreamReassembly>,
        mut streamid_tx: watch::Sender<StreamID>,
    ) {
        tracyrs::zone!("ingest_streams");
        let mut client_buf = Vec::new();
        let mut server_buf = Vec::new();
        loop {
            let stream = rx.next().await.unwrap();
            tracyrs::zone!("ingest_streams", "ingesting stream");
            stream
                .finalize(&*self, &mut client_buf, &mut server_buf, &mut streamid_tx)
                .await;
        }
    }
}
