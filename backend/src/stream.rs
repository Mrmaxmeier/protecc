use std::collections::{HashMap, HashSet};
use std::net::IpAddr;

use serde::{Deserialize, Serialize};

use crate::database::{Database, Segment, Sender, StreamID, StreamPayloadID, TagID};
use crate::reassembly::StreamReassembly;

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

impl Stream {
    pub(crate) async fn from(stream: StreamReassembly, db: &Database) -> Self {
        let StreamReassembly {
            client,
            server,
            client_to_server,
            server_to_client,
            ..
        } = stream;

        let mut client_data = Vec::new();
        let mut server_data = Vec::new();
        client_to_server.flatten_into(&mut client_data);
        server_to_client.flatten_into(&mut server_data);

        let mut packets = client_to_server
            .packets
            .iter()
            .map(|p| (Sender::Client, p))
            .chain(server_to_client.packets.iter().map(|p| (Sender::Server, p)))
            .collect::<Vec<_>>();

        // TODO: better sort
        packets.sort_by(|a, b| a.1.timestamp.cmp(&b.1.timestamp));

        let mut segments = Vec::with_capacity(packets.len());
        let mut client_pos = 0;
        let mut server_pos = 0;
        for (sender, packet) in packets.into_iter() {
            if packet.data.is_empty() {
                continue;
            } // TODO: does this break things? check for PSH?
            let pos = match sender {
                Sender::Client => client_pos,
                Sender::Server => server_pos,
            };
            match sender {
                Sender::Client => client_pos += packet.data.len(),
                Sender::Server => server_pos += packet.data.len(),
            }
            segments.push(crate::database::Segment {
                sender,
                start: pos,
                flags: 0,
                timestamp: 0,
            });
        }

        tracyrs::zone!("Database::push_raw");
        let client_data_id = db.store_data(&client_data);
        let server_data_id = db.store_data(&server_data);
        Stream {
            id: StreamID::new(0),
            client,
            server,
            segments,
            client_data_len: client_data.len() as u32,
            server_data_len: server_data.len() as u32,
            client_data_id,
            server_data_id,
            tags: HashSet::new(),
            features: HashMap::new(),
        }
    }

    pub(crate) fn as_lightweight(&self) -> LightweightStream {
        LightweightStream {
            id: self.id.clone(),
            client: self.client.clone(),
            server: self.server.clone(),
            tags: self.tags.clone(),
            client_data_len: self.client_data_len,
            server_data_len: self.server_data_len,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LightweightStream {
    pub(crate) id: StreamID,
    pub(crate) client: (IpAddr, u16),
    pub(crate) server: (IpAddr, u16),
    pub(crate) tags: HashSet<TagID>,
    pub(crate) client_data_len: u32,
    pub(crate) server_data_len: u32,
}
