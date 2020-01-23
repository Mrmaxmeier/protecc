use pcap_parser::data::{PacketData, ETHERTYPE_IPV4, ETHERTYPE_IPV6};
use pcap_parser::traits::PcapReaderIterator;
use pcap_parser::{Block, LegacyPcapReader, PcapBlockOwned, PcapError, PcapNGReader};
use pktparse;
use std::fs::File;
use std::io::Read;
use std::net::IpAddr;
use std::path::Path;
use std::time::{Duration, SystemTime};

use crate::incr_counter;
use crate::reassembly::{Packet, Reassembler};

fn handle_packetdata(packet: PacketData) -> Option<Packet> {
    tracyrs::zone!("handle_packetdata");
    match packet {
        PacketData::L2(payload) => handle_l2(payload),
        PacketData::L3(ETHERTYPE_IPV4, payload) => handle_ip4(payload),
        PacketData::L3(ETHERTYPE_IPV6, payload) => handle_ip6(payload),
        _ => {
            incr_counter!(packets_unhandled);
            None
        }
    }
}

fn handle_l2(data: &[u8]) -> Option<Packet> {
    let (payload, header) = pktparse::ethernet::parse_ethernet_frame(data).unwrap();
    use pktparse::ethernet::EtherType;
    match header.ethertype {
        EtherType::IPv4 => handle_ip4(payload),
        EtherType::IPv6 => handle_ip6(payload),
        _ => {
            incr_counter!(packets_unhandled);
            None
        }
    }
}

fn handle_ip4(data: &[u8]) -> Option<Packet> {
    if let Ok((payload, header)) = pktparse::ipv4::parse_ipv4_header(data) {
        use pktparse::ip::IPProtocol;
        match header.protocol {
            IPProtocol::TCP => {
                let addrs = (header.source_addr.into(), header.dest_addr.into());
                return handle_tcp(payload, addrs);
            }
            _ => incr_counter!(packets_unhandled),
        }
    } else {
        incr_counter!(packets_malformed);
    }
    None
}
fn handle_ip6(data: &[u8]) -> Option<Packet> {
    if let Ok((payload, header)) = pktparse::ipv6::parse_ipv6_header(data) {
        use pktparse::ip::IPProtocol;
        match header.next_header {
            IPProtocol::TCP => {
                let addrs = (header.source_addr.into(), header.dest_addr.into());
                return handle_tcp(payload, addrs);
            }
            _ => incr_counter!(packets_unhandled),
        }
    } else {
        incr_counter!(packets_malformed);
    }
    None
}

fn handle_tcp(data: &[u8], addrs: (IpAddr, IpAddr)) -> Option<Packet> {
    if let Ok((payload, header)) = pktparse::tcp::parse_tcp_header(data) {
        incr_counter!(packets_tcp);
        let packet = Packet {
            src_ip: addrs.0,
            dst_ip: addrs.1,
            data: payload.into(),
            tcp_header: header,
            timestamp: None,
        };
        Some(packet)
    } else {
        incr_counter!(packets_malformed);
        None
    }
}

pub(crate) async fn read_pcap_file(path: &Path, reassembler: &mut Reassembler) {
    //    tracyrs::zone!("pcap_read_file");
    let start = std::time::Instant::now();

    let file = File::open(path).expect("unable to open pcap file");

    let mut filename = path
        .file_name()
        .expect("malformed pcap path")
        .to_str()
        .unwrap()
        .to_owned();

    let reader = if filename.ends_with(".zst") {
        filename.truncate(filename.len() - 4);
        Box::new(zstd::stream::read::Decoder::new(file).unwrap()) as Box<dyn Read + Send>
    } else {
        dbg!(&path);
        Box::new(file) as Box<dyn Read + Send>
    };

    let mut reader = if filename.ends_with(".pcapng") {
        Box::new(PcapNGReader::new(1 << 20, reader).expect("PcapNGReader"))
            as Box<dyn PcapReaderIterator<Box<dyn Read + Send>> + Send>
    } else {
        Box::new(LegacyPcapReader::new(1 << 20, reader).expect("LegacyPcapReader"))
            as Box<dyn PcapReaderIterator<Box<dyn Read + Send>> + Send>
    };
    let mut if_linktypes = Vec::new();
    let mut if_tsconfig = Vec::new();
    loop {
        match {
            //tracyrs::zone!("pcap_read_file", "reader.next");
            reader.next()
        } {
            Ok((offset, block)) => {
                incr_counter!(pcap_blocks);
                //                tracyrs::zone!("pcap_read_file", "pcap process block");
                match block {
                    PcapBlockOwned::NG(Block::SectionHeader(ref _shb)) => {
                        // starting a new section, clear known interfaces
                        if_linktypes = Vec::new();
                    }
                    PcapBlockOwned::NG(Block::InterfaceDescription(ref idb)) => {
                        if_linktypes.push(idb.linktype);
                        if_tsconfig.push((idb.if_tsoffset, idb.if_tsresol));
                    }
                    PcapBlockOwned::NG(Block::EnhancedPacket(ref epb)) => {
                        assert!((epb.if_id as usize) < if_linktypes.len(), "invalid pcapng");
                        let (ts_offset, ts_resol) = if_tsconfig[epb.if_id as usize];
                        let (ts_secs, ts_frac, frac_unit) = pcap_parser::pcapng::build_ts(
                            epb.ts_high,
                            epb.ts_low,
                            ts_offset,
                            ts_resol,
                        );
                        let ts = SystemTime::UNIX_EPOCH
                            .checked_add(
                                ts_secs * Duration::SECOND
                                    + ts_frac * Duration::from_nanos(1_000_000_000 / frac_unit),
                            )
                            .expect("pcap timestamp overflow");
                        let linktype = if_linktypes[epb.if_id as usize];
                        let res = pcap_parser::data::get_packetdata(
                            epb.data,
                            linktype,
                            epb.caplen as usize,
                        );
                        crate::counters::update_counters(|c| c.packet_bytes += epb.caplen as u64);
                        if let Some(res) = res {
                            if let Some(mut packet) = handle_packetdata(res) {
                                packet.timestamp = Some(ts);
                                reassembler.advance_state(packet).await;
                            }
                        } else {
                            panic!("packet without data");
                        }
                    }
                    PcapBlockOwned::NG(Block::SimplePacket(ref _spb)) => {
                        todo!("how should we handle timestamps for SimplePacketBlocks?");
                        /*
                        assert!(!if_linktypes.is_empty());
                        let linktype = if_linktypes[0];
                        let blen = (spb.block_len1 - 16) as usize;
                        let res = pcap_parser::data::get_packetdata(spb.data, linktype, blen);
                        if let Some(res) = res {
                            let _: () = handle_packetdata(res);
                        } else {
                            panic!("packet without data");
                        }
                        */
                    }
                    PcapBlockOwned::NG(_) => {
                        // can be statistics (ISB), name resolution (NRB), etc.
                        panic!("unsupported pcapng block");
                    }
                    PcapBlockOwned::Legacy(block) => {
                        let ts = SystemTime::UNIX_EPOCH
                            .checked_add(
                                block.ts_sec * Duration::SECOND
                                    + block.ts_usec * Duration::MICROSECOND,
                            )
                            .expect("pcap timestamp overflow");
                        let linktype = if_linktypes[0];
                        let res = pcap_parser::data::get_packetdata(
                            block.data,
                            linktype,
                            block.caplen as usize,
                        );
                        crate::counters::update_counters(|c| c.packet_bytes += block.caplen as u64);
                        if let Some(res) = res {
                            if let Some(mut packet) = handle_packetdata(res) {
                                packet.timestamp = Some(ts);
                                reassembler.advance_state(packet).await;
                            }
                        } else {
                            panic!("packet without data");
                        }
                    }
                    PcapBlockOwned::LegacyHeader(header) => {
                        if_linktypes.push(header.network);
                    }
                }
                {
                    tracyrs::zone!("read_pcap_file", "reader.consume");
                    reader.consume(offset);
                }
            }
            Err(PcapError::Eof) => break,
            Err(PcapError::Incomplete) => {
                tracyrs::zone!("read_pcap_file", "reader.refill");
                reader.refill().unwrap();
            }
            Err(e) => panic!("error while reading: {:?}", e),
        }
    }
    incr_counter!(pcaps_imported);
    crate::counters::update_counters(|c| {
        c.pcap_processing_milliseconds += start.elapsed().as_millis() as u64
    });
}
