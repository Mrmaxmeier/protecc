use pcap_parser::data::PacketData;
use pcap_parser::traits::PcapReaderIterator;
use pcap_parser::{Block, PcapBlockOwned, PcapError, PcapNGReader};
use pktparse;
use std::fs::File;
use std::net::IpAddr;

use crate::reassembly::{Packet, Reassembler};

fn handle_l2(reassembler: &mut Reassembler, data: &[u8]) {
    let (payload, header) = pktparse::ethernet::parse_ethernet_frame(data).unwrap();
    use pktparse::ethernet::EtherType;
    match header.ethertype {
        EtherType::IPv4 => handle_ip4(reassembler, payload),
        EtherType::IPv6 => handle_ip6(reassembler, payload),
        _ => {
            // TODO: incr counter
        }
    }
}

fn handle_ip4(reassembler: &mut Reassembler, data: &[u8]) {
    if let Ok((payload, header)) = pktparse::ipv4::parse_ipv4_header(data) {
        use pktparse::ip::IPProtocol;
        match header.protocol {
            IPProtocol::TCP => {
                let addrs = (header.source_addr.into(), header.dest_addr.into());
                handle_tcp(reassembler, payload, addrs);
            }
            _ => {
                // TODO: incr counter
            }
        }
    } else {
        // TODO: incr counter
    }
}
fn handle_ip6(reassembler: &mut Reassembler, data: &[u8]) {
    if let Ok((payload, header)) = pktparse::ipv6::parse_ipv6_header(data) {
        use pktparse::ip::IPProtocol;
        match header.next_header {
            IPProtocol::TCP => {
                let addrs = (header.source_addr.into(), header.dest_addr.into());
                handle_tcp(reassembler, payload, addrs);
            }
            _ => {
                // TODO: incr counter
            }
        }
    } else {
        // TODO: incr counter
    }
}

fn handle_tcp(reassembler: &mut Reassembler, data: &[u8], addrs: (IpAddr, IpAddr)) {
    if let Ok((payload, header)) = pktparse::tcp::parse_tcp_header(data) {
        let packet = Packet {
            src_ip: addrs.0,
            dst_ip: addrs.1,
            data: payload.into(),
            tcp_header: header,
            timestamp: 0,
        };
        reassembler.advance_state(packet);
    } else {
        // TODO: incr counter
    }
}

pub(crate) fn read_pcap_file(path: &str, reassembler: &mut Reassembler) {
    let file = File::open(path).unwrap();
    let mut num_blocks = 0;
    let mut reader = PcapNGReader::new(65536, file).expect("PcapNGReader");
    let mut if_linktypes = Vec::new();
    loop {
        match reader.next() {
            Ok((offset, block)) => {
                num_blocks += 1;
                match block {
                    PcapBlockOwned::NG(Block::SectionHeader(ref _shb)) => {
                        // starting a new section, clear known interfaces
                        if_linktypes = Vec::new();
                    }
                    PcapBlockOwned::NG(Block::InterfaceDescription(ref idb)) => {
                        if_linktypes.push(idb.linktype);
                    }
                    PcapBlockOwned::NG(Block::EnhancedPacket(ref epb)) => {
                        assert!((epb.if_id as usize) < if_linktypes.len(), "invalid pcapng");
                        let linktype = if_linktypes[epb.if_id as usize];
                        let res = pcap_parser::data::get_packetdata(
                            epb.data,
                            linktype,
                            epb.caplen as usize,
                        );
                        match res {
                            Some(PacketData::L2(eth)) => {
                                handle_l2(reassembler, eth);
                            }
                            _ => {}
                        }
                    }
                    PcapBlockOwned::NG(Block::SimplePacket(ref spb)) => {
                        assert!(if_linktypes.len() > 0);
                        let linktype = if_linktypes[0];
                        let blen = (spb.block_len1 - 16) as usize;
                        let res = pcap_parser::data::get_packetdata(spb.data, linktype, blen);
                        match res {
                            Some(PacketData::L2(x)) => {
                                dbg!(x);
                            }
                            _ => {}
                        }
                        //reassembler.advance_state(res);
                    }
                    PcapBlockOwned::NG(_) => {
                        // can be statistics (ISB), name resolution (NRB), etc.
                        eprintln!("unsupported block");
                    }
                    PcapBlockOwned::Legacy(_) | PcapBlockOwned::LegacyHeader(_) => unreachable!(),
                }
                reader.consume(offset);
            }
            Err(PcapError::Eof) => break,
            Err(PcapError::Incomplete) => {
                reader.refill().unwrap();
            }
            Err(e) => panic!("error while reading: {:?}", e),
        }
    }
}
