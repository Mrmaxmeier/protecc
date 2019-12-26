use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Arc;

use pktparse::tcp::TcpHeader;
use std::cmp::max;

use crate::database;
use crate::incr_counter;

use crate::database::Database;

#[derive(Debug)]
pub(crate) struct Packet {
    pub(crate) src_ip: IpAddr,
    pub(crate) dst_ip: IpAddr,
    pub(crate) timestamp: u64,
    pub(crate) tcp_header: TcpHeader,
    pub(crate) data: Vec<u8>,
}

struct Stream {
    unacked: Vec<Packet>,
    highest_ack: Option<u32>,
    is_closed: bool,
    latest_packet: Option<u64>,
    packets: Vec<Packet>,
}
impl Stream {
    fn new() -> Self {
        Stream {
            unacked: Default::default(),
            highest_ack: None,
            is_closed: false,
            latest_packet: None,
            packets: vec![],
        }
    }

    fn add(&mut self, p: Packet) {
        self.latest_packet = Some(max(self.latest_packet.unwrap_or(0), p.timestamp));
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

    fn flattened(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        for pkt in &self.packets {
            buf.extend(&pkt.data);
        }
        buf
    }
}

struct StreamReassembly {
    server: (IpAddr, u16),
    client: (IpAddr, u16),
    client_to_server: Stream,
    server_to_client: Stream,
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
    fn finalize(self, db: &Database) {
        let StreamReassembly {
            client,
            server,
            client_to_server,
            server_to_client,
            ..
        } = self;

        let client_data = client_to_server.flattened();
        let server_data = server_to_client.flattened();

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
            if packet.data.is_empty() { continue } // TODO: does this break things? check for PSH?
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

        db.push_raw(client, server, segments, client_data, server_data)
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
    database: Arc<Database>,
}
impl Reassembler {
    pub(crate) fn new(database: Arc<Database>) -> Self {
        Reassembler {
            reassemblies: HashMap::new(),
            database,
        }
    }

    pub(crate) fn advance_state(&mut self, p: Packet) {
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
            stream.finalize(&self.database);
        }
    }

    pub(crate) fn expire(&mut self) {
        let mut wip_bytes = 0;
        for (_, r) in self.reassemblies.iter() {
            for pkt in &r.client_to_server.packets {
                wip_bytes += pkt.data.len();
            }
            for pkt in &r.server_to_client.packets {
                wip_bytes += pkt.data.len();
            }
        }
        // dbg!(self.reassemblies.len());
        // dbg!(wip_bytes);
        // TODO: implement this
        self.reassemblies.clear();
    }
}
