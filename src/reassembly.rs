use std::collections::HashMap;
use std::net::IpAddr;

use pktparse::tcp::TcpHeader;
use std::cmp::max;

#[derive(Debug, Copy)]
struct Packet<'a> {
    src_ip: IpAddr,
    dst_ip: IpAddr,
    timestamp: u64,
    tcp_header: TcpHeader,
    data: &'a [u8],
}

struct Stream<'a> {
    unacked: Vec<&'a Packet<'a>>,
    highest_ack: u32,
    is_closed: bool,
    latest_packet: u64,
    packets: Vec<&'a Packet<'a>>,
}
impl Stream<'_> {
    fn new() -> Self {
        Stream {
            unacked: Default::default(),
            highest_ack: -1,
            is_closed: false,
            latest_packet: -1,
            packets: vec![],
        }
    }

    fn add(&mut self, p: &Packet) {
        self.packets.push(p);
        self.latest_packet = max(self.latest_packet, p.timestamp);
        if p.tcp_header.sequence_no >= self.highest_ack
            && (p.tcp_header.flag_psh || p.tcp_header.flag_syn || p.tcp_header.flag_fin)
        {
            self.unacked.push(p)
        }
    }

    fn ack(&mut self, ack_number: u32) {
        self.highest_ack = max(self.highest_ack, ack_number);
        self.is_closed = self
            .unacked
            .into_iter()
            .any(|p| p.tcp_header.sequence_no < ack_number && p.tcp_header.flag_fin);
        self.unacked = self
            .unacked
            .drain_filter(|p| p.tcp_header.sequence_no >= ack_number)
            .collect();
    }
}

struct StreamReassembly<'a> {
    server: (IpAddr, u16),
    client: (IpAddr, u16),
    client_to_server: Stream<'a>,
    server_to_client: Stream<'a>,
    reset: bool,
}
impl StreamReassembly<'_> {
    fn get_stream(&self, p: &Packet) -> &Stream {
        if (p.src_ip, p.tcp_header.source_port) == self.server {
            &self.server_to_client
        } else {
            &self.client_to_server
        }
    }
    fn get_other_stream(&self, p: &Packet) -> &Stream {
        if (p.src_ip, p.tcp_header.source_port) == self.server {
            &self.client_to_server
        } else {
            &self.server_to_client
        }
    }
    fn add(&mut self, p: &Packet) {
        if p.tcp_header.flag_rst {
            self.reset = true
        }
        self.get_stream(p).add(p);
        if p.tcp_header.flag_ack {
            self.get_other_stream(p).ack(p.tcp_header.ack_no)
        }
    }
    fn is_done(&self) -> bool {
        self.reset || (self.client_to_server.is_closed && self.server_to_client.is_closed)
    }
    fn packets(&self) -> (Vec<&Packet>, Vec<&Packet>) {
        (
            self.client_to_server.packets.clone(),
            self.server_to_client.packets.clone(),
        )
    }
}

#[derive(Eq, PartialEq, Hash)]
struct StreamId(IpAddr, u16, IpAddr, u16);
impl StreamId {
    fn new(src: IpAddr, src_port: u16, dst: IpAddr, dst_port: u16) -> Self {
        if (src, src_port) > (dst, dst_port) {
            StreamId(src, src_port, dst, dst_port)
        } else {
            StreamId(src, src_port, dst, dst_port)
        }
    }
}

struct Reassembler<'a> {
    reassemblies: HashMap<StreamId, StreamReassembly<'a>>,
}
impl Reassembler<'_> {
    fn new() -> Self {
        Reassembler {
            reassemblies: HashMap::new(),
        }
    }

    fn advance_state(&mut self, p: Packet) -> Option<(Vec<&Packet>, Vec<&Packet>)> {
        let id = StreamId(
            p.srcIp,
            p.tcp_header.source_port,
            p.dstIp,
            p.tcp_header.dest_port,
        );
        if !self.reassemblies.contains_key(&id) {
            if !p.tcp_header.flag_syn {
                println!("Packet that does not belong to a stream: {:?}", p);
                return None;
            }
        }
        self.reassemblies.insert(
            id,
            StreamReassembly {
                server: (p.dst_ip, p.tcp_header.dest_port),
                client: (p.src_ip, p.tcp_header.source_port),
                client_to_server: Stream::new(),
                server_to_client: Stream::new(),
                reset: false,
            },
        );

        let mut reassembly = self
            .reassemblies
            .get(&id)
            .expect("This should literally never happen");

        reassembly.add(&p);
        if reassembly.is_done() {
            self.reassemblies.remove(&id);
            return Some(reassembly.packets());
        }
        return None;
    }
}
