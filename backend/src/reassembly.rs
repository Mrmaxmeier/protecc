use crate::database::Database;
use crate::incr_counter;
use crate::workq::WorkQ;
use arbitrary::{Arbitrary, Result, Unstructured};
use pktparse::tcp::TcpHeader;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::net::IpAddr;
use std::sync::Arc;
use std::time::SystemTime;

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

#[derive(Debug, Arbitrary)]
pub(crate) struct TinyTcpHeader {
    pub(crate) seq_no: u32,
    pub(crate) ack_no: u32,

    pub(crate) flag_urg: bool,
    pub(crate) flag_ack: bool,
    pub(crate) flag_psh: bool,
    pub(crate) flag_rst: bool,
    pub(crate) flag_syn: bool,
    pub(crate) flag_fin: bool,
}

impl TinyTcpHeader {
    fn from_tcpheader(th: &TcpHeader) -> Self {
        TinyTcpHeader {
            flag_urg: th.flag_urg,
            flag_ack: th.flag_ack,
            flag_psh: th.flag_psh,
            flag_rst: th.flag_rst,
            flag_syn: th.flag_syn,
            flag_fin: th.flag_fin,
            ack_no: th.ack_no,
            seq_no: th.sequence_no,
        }
    }
    pub fn flags_as_u8(&self) -> u8 {
        let mut flags = 0;
        flags |= (self.flag_urg as u8) << 5;
        flags |= (self.flag_ack as u8) << 4;
        flags |= (self.flag_psh as u8) << 3;
        flags |= (self.flag_rst as u8) << 2;
        flags |= (self.flag_syn as u8) << 1;
        flags |= (self.flag_fin as u8) << 0;
        flags
    }
}

#[derive(Debug, Arbitrary)]
pub(crate) struct SPacket {
    pub(crate) timestamp: Option<u64>,
    pub(crate) tcp_header: TinyTcpHeader,
    pub(crate) data: Vec<u8>,
}

impl SPacket {
    pub fn from_packet(p: Packet) -> Self {
        SPacket {
            timestamp: p.timestamp.map(|ts| {
                ts.duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as u64
            }),

            tcp_header: TinyTcpHeader::from_tcpheader(&p.tcp_header),
            data: p.data,
        }
    }
}

#[derive(Debug, Arbitrary)]
pub(crate) struct Stream {
    unacked: Vec<SPacket>,
    highest_ack: Option<u32>,
    is_closed: bool,
    latest_packet: Option<u64>,
    pub(crate) packets: Vec<SPacket>, // TODO(footprint/perf): smallvec this
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
        // TODO: acked vs non-acked
        let timestamp_secs = packet_time_secs(&p);
        self.latest_packet = Some(max(self.latest_packet.unwrap_or(0), timestamp_secs));
        if p.tcp_header.sequence_no >= self.highest_ack.unwrap_or(0)
            && (!p.data.is_empty() || p.tcp_header.flag_syn || p.tcp_header.flag_fin)
        {
            self.unacked.push(SPacket::from_packet(p))
        } else {
            self.packets.push(SPacket::from_packet(p));
        }
    }

    fn ack(&mut self, ack_number: u32) {
        self.highest_ack = Some(max(self.highest_ack.unwrap_or(0), ack_number));
        self.is_closed = self.is_closed
            || self
                .unacked
                .iter()
                .any(|p| p.tcp_header.seq_no < ack_number && p.tcp_header.flag_fin);
        self.packets.extend(
            self.unacked
                .drain_filter(|p| p.tcp_header.seq_no < ack_number),
        );
    }

    pub(crate) fn remove_retransmissions(&mut self) {
        tracyrs::zone!("Stream::remove_retransmissions");
        let mut seen = HashSet::new();
        self.packets.retain(|p| {
            let th = &p.tcp_header;
            seen.insert((
                th.seq_no,
                th.ack_no,
                p.data.len(),
                th.flag_syn,
                th.flag_ack,
                th.flag_rst,
                th.flag_fin,
            ))
        });
    }
}

#[derive(Debug)]
pub struct StreamReassembly {
    pub(crate) server: (IpAddr, u16),
    pub(crate) client: (IpAddr, u16),
    pub(crate) client_to_server: Stream,
    pub(crate) server_to_client: Stream,
    pub(crate) latest_timestamp: u64,
    pub(crate) reset: bool,
    pub(crate) malformed: bool,
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
}

impl Arbitrary for StreamReassembly {
    fn arbitrary(u: &mut Unstructured<'_>) -> Result<Self> {
        use std::net::Ipv4Addr;
        let ip1 = IpAddr::V4(Ipv4Addr::new(10, 0, 0, u8::arbitrary(u)?));
        let ip2 = IpAddr::V4(Ipv4Addr::new(10, 0, 0, u8::arbitrary(u)?));
        Ok(StreamReassembly {
            server: (ip1, u16::arbitrary(u)?),
            client: (ip2, u16::arbitrary(u)?),
            client_to_server: Stream::arbitrary(u)?,
            server_to_client: Stream::arbitrary(u)?,
            latest_timestamp: u64::arbitrary(u)?,
            reset: bool::arbitrary(u)?,
            malformed: bool::arbitrary(u)?,
        })
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

pub struct Reassembler {
    reassemblies: HashMap<StreamId, StreamReassembly>,
    // recently_closed: HashMap<StreamId, u64>,
    latest_timestamp: u64,
    database_ingest: Arc<WorkQ<StreamReassembly>>,
    tcp_initiations_by_ip: HashMap<IpAddr, u64>,
}
impl Reassembler {
    pub fn new(database: Arc<Database>) -> Self {
        let database_ingest = database.streams_queue.clone();
        Reassembler {
            reassemblies: HashMap::new(),
            tcp_initiations_by_ip: HashMap::new(),
            latest_timestamp: 0,
            database_ingest,
        }
    }

    pub(crate) async fn advance_state(&mut self, p: Packet) {
        // tracyrs::zone!("Reassembler::advance_state");
        let timestamp_secs = packet_time_secs(&p);
        debug_assert!(timestamp_secs >= self.latest_timestamp);
        self.latest_timestamp = timestamp_secs;

        let id = StreamId::new(
            p.src_ip,
            p.tcp_header.source_port,
            p.dst_ip,
            p.tcp_header.dest_port,
        );
        if !self.reassemblies.contains_key(&id) {
            let mut client = (p.src_ip, p.tcp_header.source_port);
            let mut server = (p.dst_ip, p.tcp_header.dest_port);
            if !p.tcp_header.flag_syn {
                if p.data.is_empty() {
                    incr_counter!(packets_without_stream);
                    return;
                }

                let client_syns = self.tcp_initiations_by_ip.get(&client.0).unwrap_or(&0);
                let server_syns = self.tcp_initiations_by_ip.get(&server.0).unwrap_or(&0);
                if server_syns > client_syns {
                    std::mem::swap(&mut client, &mut server);
                }
                /*
                eprintln!(
                    "Packet that does not belong to a stream: {:?} -> {:?} ({} -> {}). Server port guess: {}",
                    p.src_ip, p.dst_ip, p.tcp_header.source_port, p.tcp_header.dest_port, server.1
                );
                */
                incr_counter!(streams_without_syn);
            } else {
                *self.tcp_initiations_by_ip.entry(client.0).or_default() += 1;
            }
            self.reassemblies.insert(
                id.clone(),
                StreamReassembly {
                    server,
                    client,
                    client_to_server: Stream::new(),
                    server_to_client: Stream::new(),
                    reset: false,
                    latest_timestamp: timestamp_secs,
                    malformed: !p.tcp_header.flag_syn,
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
            let stream = self.reassemblies.remove(&id).unwrap();
            self.submit_stream(id, stream).await;
        }
    }

    async fn submit_stream(&mut self, _id: StreamId, stream: StreamReassembly) {
        if stream.server_to_client.packets.is_empty() {
            incr_counter!(streams_discarded_no_server_packets);
            return;
        }
        let any_server_payload = stream
            .server_to_client
            .packets
            .iter()
            .any(|x| !x.data.is_empty());
        let any_client_payload = stream
            .client_to_server
            .packets
            .iter()
            .any(|x| !x.data.is_empty());
        if !any_server_payload && !any_client_payload {
            incr_counter!(streams_discarded_no_data);
            return;
        }
        incr_counter!(streams_completed);
        self.database_ingest.push(stream).await;
        // self.recently_closed.insert(id, timestamp_secs);
    }

    pub async fn expire(&mut self) {
        //        tracyrs::zone!("Reassembler::expire");
        let reassemblies = std::mem::replace(&mut self.reassemblies, HashMap::new());
        for (id, stream) in reassemblies.into_iter() {
            if stream.latest_timestamp + PACKET_EXPIRE_THRESHOLD_SECS < self.latest_timestamp {
                incr_counter!(streams_timeout_expired);
                self.submit_stream(id, stream).await;
            } else {
                self.reassemblies.insert(id, stream);
            }
        }
    }
}
