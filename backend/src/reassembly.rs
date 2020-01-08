use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;
use std::time::SystemTime;
use tokio::sync::mpsc;

use pktparse::tcp::TcpHeader;
use std::cmp::max;

use crate::database;
use crate::incr_counter;

use crate::database::Database;

const PACKET_EXPIRE_THRESHOLD_SECS: u64 = 60 * 5;

fn packet_time_secs(p: &Packet) -> u64 {
    p.timestamp
        .unwrap()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[derive(Debug)]
pub(crate) struct Packet {
    pub(crate) src_ip: IpAddr,
    pub(crate) dst_ip: IpAddr,
    pub(crate) timestamp: Option<SystemTime>,
    pub(crate) tcp_header: TcpHeader,
    pub(crate) data: Vec<u8>,
}

#[derive(Debug)]
struct Stream {
    unacked: Vec<Packet>,
    highest_ack: Option<u32>,
    is_closed: bool,
    latest_packet: Option<u64>,
    packets: Vec<Packet>, // TODO(footprint/perf): smallvec this
}

impl Stream {
    fn new() -> Self {
        Stream {
            unacked: Vec::new(),
            highest_ack: None,
            is_closed: false,
            latest_packet: None,
            packets: Vec::with_capacity(4),
        }
    }

    fn add(&mut self, p: Packet) {
        tracyrs::zone!("Stream::add");
        // TODO: acked vs non-acked
        let timestamp_secs = packet_time_secs(&p);
        self.latest_packet = Some(max(self.latest_packet.unwrap_or(0), timestamp_secs));
        if p.tcp_header.sequence_no >= self.highest_ack.unwrap_or(0)
            && (p.tcp_header.flag_psh || p.tcp_header.flag_syn || p.tcp_header.flag_fin)
        {
            self.unacked.push(p)
        } else {
            self.packets.push(p);
        }
    }

    fn ack(&mut self, ack_number: u32) {
        self.highest_ack = Some(max(self.highest_ack.unwrap_or(0), ack_number));
        self.is_closed = self
            .unacked
            .iter()
            .any(|p| p.tcp_header.sequence_no < ack_number && p.tcp_header.flag_fin);
        self.packets.extend(
            self.unacked
                .drain_filter(|p| p.tcp_header.sequence_no < ack_number),
        );
    }

    fn flatten_into(&self, buffer: &mut Vec<u8>) {
        for pkt in &self.packets {
            buffer.extend(&pkt.data);
        }
    }
}

#[derive(Debug)]
pub(crate) struct StreamReassembly {
    server: (IpAddr, u16),
    client: (IpAddr, u16),
    client_to_server: Stream,
    server_to_client: Stream,
    latest_timestamp: u64,
    reset: bool,
}
impl StreamReassembly {
    fn get_stream(&mut self, p: &Packet) -> &mut Stream {
        if (p.src_ip, p.tcp_header.source_port) == self.server {
            &mut self.server_to_client
        } else {
            &mut self.client_to_server
        }
    }
    fn get_other_stream(&mut self, p: &Packet) -> &mut Stream {
        if (p.src_ip, p.tcp_header.source_port) == self.server {
            &mut self.client_to_server
        } else {
            &mut self.server_to_client
        }
    }
    fn add(&mut self, p: Packet) {
        let timestamp_secs = packet_time_secs(&p);
        self.latest_timestamp = timestamp_secs;
        if p.tcp_header.flag_rst {
            self.reset = true
        }
        if p.tcp_header.flag_ack {
            self.get_other_stream(&p).ack(p.tcp_header.ack_no)
        }
        self.get_stream(&p).add(p);
    }
    fn is_done(&self) -> bool {
        self.reset || (self.client_to_server.is_closed && self.server_to_client.is_closed)
    }

    pub(crate) async fn finalize(
        self,
        db: &Database,
        _flat_client: &mut Vec<u8>,
        _flat_server: &mut Vec<u8>,
    ) {
        let StreamReassembly {
            client,
            server,
            client_to_server,
            server_to_client,
            ..
        } = self;

        _flat_client.clear();
        _flat_server.clear();
        client_to_server.flatten_into(_flat_client);
        server_to_client.flatten_into(_flat_server);

        let mut packets = client_to_server
            .packets
            .iter()
            .map(|p| (database::Sender::Client, p))
            .chain(
                server_to_client
                    .packets
                    .iter()
                    .map(|p| (database::Sender::Server, p)),
            )
            .collect::<Vec<_>>();

        packets.sort_by(|a, b| a.1.timestamp.cmp(&b.1.timestamp));

        let mut segments = Vec::with_capacity(packets.len());
        let mut client_pos = 0;
        let mut server_pos = 0;
        for (sender, packet) in packets.into_iter() {
            if packet.data.is_empty() {
                continue;
            } // TODO: does this break things? check for PSH?
            use database::Sender::*;
            let pos = match sender {
                Client => client_pos,
                Server => server_pos,
            };
            match sender {
                Client => client_pos += packet.data.len(),
                Server => server_pos += packet.data.len(),
            }
            segments.push((sender, pos));
        }

        db.push_raw(client, server, segments, _flat_client, _flat_server)
            .await;
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
struct StreamId(IpAddr, u16, IpAddr, u16);
impl StreamId {
    fn new(src: IpAddr, src_port: u16, dst: IpAddr, dst_port: u16) -> Self {
        if (src, src_port) > (dst, dst_port) {
            StreamId(dst, dst_port, src, src_port)
        } else {
            StreamId(src, src_port, dst, dst_port)
        }
    }
}

pub(crate) struct Reassembler {
    reassemblies: HashMap<StreamId, StreamReassembly>,
    latest_timestamp: u64,
    database_ingest: mpsc::Sender<StreamReassembly>,
}
impl Reassembler {
    pub(crate) fn new(database: Arc<Database>) -> Self {
        let database_ingest = database.ingest_tx.clone();
        Reassembler {
            reassemblies: HashMap::new(),
            latest_timestamp: 0,
            database_ingest,
        }
    }

    pub(crate) async fn advance_state(&mut self, p: Packet) {
        tracyrs::zone!("Reassembler::advance_state");
        let timestamp_secs = packet_time_secs(&p);
        assert!(timestamp_secs >= self.latest_timestamp);
        self.latest_timestamp = timestamp_secs;

        let id = StreamId::new(
            p.src_ip,
            p.tcp_header.source_port,
            p.dst_ip,
            p.tcp_header.dest_port,
        );
        if !self.reassemblies.contains_key(&id) {
            if !p.tcp_header.flag_syn {
                // eprintln!("Packet that does not belong to a stream: {:?} -> {:?} ({} -> {})", p.src_ip, p.dst_ip, p.tcp_header.source_port, p.tcp_header.dest_port);
                incr_counter!(packets_without_stream);
                return;
            }
            self.reassemblies.insert(
                id.clone(),
                StreamReassembly {
                    server: (p.dst_ip, p.tcp_header.dest_port),
                    client: (p.src_ip, p.tcp_header.source_port),
                    client_to_server: Stream::new(),
                    server_to_client: Stream::new(),
                    reset: false,
                    latest_timestamp: timestamp_secs,
                },
            );
        }

        let is_done = {
            let reassembly = self
                .reassemblies
                .get_mut(&id)
                .expect("This should literally never happen");

            reassembly.add(p);
            reassembly.is_done()
        };
        if is_done {
            incr_counter!(streams_completed);
            let stream = self.reassemblies.remove(&id).unwrap();
            tracyrs::zone!("database_ingest.send");
            self.database_ingest.send(stream).await.unwrap();
        }
    }

    pub(crate) async fn expire(&mut self) {
        tracyrs::zone!("Reassembler::expire");
        let reassemblies = std::mem::replace(&mut self.reassemblies, HashMap::new());
        for (key, stream) in reassemblies.into_iter() {
            if stream.latest_timestamp + PACKET_EXPIRE_THRESHOLD_SECS < self.latest_timestamp {
                incr_counter!(streams_timeout_expired);
                tracyrs::zone!("database_ingest.send");
                self.database_ingest.send(stream).await.unwrap();
            } else {
                self.reassemblies.insert(key, stream);
            }
        }
    }
}
