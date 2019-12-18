use std::collections::HashMap;
use std::net::IpAddr;

use pktparse::tcp::TcpHeader;
use std::cmp::max;

#[derive(Debug)]
struct Packet {
    src_ip: IpAddr,
    dst_ip: IpAddr,
    timestamp: u64,
    tcp_header: TcpHeader,
    data: Vec<u8>,
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
    fn packets(self) -> (Vec<Packet>, Vec<Packet>) {
        (self.client_to_server.packets, self.server_to_client.packets)
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
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

struct Reassembler {
    reassemblies: HashMap<StreamId, StreamReassembly>,
}
impl Reassembler {
    fn new() -> Self {
        Reassembler {
            reassemblies: HashMap::new(),
        }
    }

    fn advance_state(&mut self, p: Packet) -> Option<(Vec<Packet>, Vec<Packet>)> {
        let id = StreamId(
            p.src_ip,
            p.tcp_header.source_port,
            p.dst_ip,
            p.tcp_header.dest_port,
        );
        if !self.reassemblies.contains_key(&id) {
            if !p.tcp_header.flag_syn {
                println!("Packet that does not belong to a stream: {:?}", p);
                return None;
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
            return self.reassemblies.remove(&id).map(StreamReassembly::packets);
        }
        return None;
    }
}
