use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::net::IpAddr;

use serde::{Deserialize, Serialize};

use crate::database::{Database, Segment, Sender, StreamID, StreamPayloadID, TagID};
use crate::reassembly::{Packet, StreamReassembly};

/*
TODO(perf/footprint):
- replace Vec with smallvec
- replace hashset with same-alloc set (tinyset)
- replace (sender, usize) with u32 & 0x7fffffff
- replace hashmap with smallvec?

MAYBE(footprint):
- global cache of ipaddrs
*/

// TODO(perf): keep stream convert allocations somewhere

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
    pub(crate) fn skeleton_from(stream: &StreamReassembly, id: StreamID) -> Self {
        Stream {
            id,
            client: stream.client.clone(),
            server: stream.server.clone(),
            segments: Vec::new(),
            client_data_len: 0,
            server_data_len: 0,
            client_data_id: StreamPayloadID(0),
            server_data_id: StreamPayloadID(0),
            tags: HashSet::new(),
            features: HashMap::new(),
        }
    }

    pub(crate) fn reconstruct_segments(stream: StreamReassembly) -> StreamSegmentResult {
        tracyrs::zone!("reconstruct_segments");
        let StreamReassembly {
            mut client_to_server,
            mut server_to_client,
            mut malformed,
            ..
        } = stream;

        client_to_server.remove_retransmissions();
        server_to_client.remove_retransmissions();

        if let Err(()) =
            make_sequence_numbers_relative(&mut client_to_server, &mut server_to_client)
        {
            malformed = true;
        }

        let mut client_data = Vec::new();
        let mut server_data = Vec::new();

        let mut packets = client_to_server
            .packets
            .iter()
            .map(|p| (Sender::Client, p))
            .chain(server_to_client.packets.iter().map(|p| (Sender::Server, p)))
            .collect::<Vec<_>>();

        packets.sort_by_key(|p| p.1.timestamp);
        packets.sort_by_key(|p| p.1.tcp_header.ack_no);
        packets.sort_by_key(|p| p.1.tcp_header.sequence_no);

        /*
        for (i, (s, p)) in packets.iter().enumerate() {
            println!(
                "{}: {:?} {} {} {}",
                i, s, p.tcp_header.sequence_no, p.tcp_header.ack_no, p.tcp_header.flag_ack
            );
        }
        */

        let mut client_packets = packets
            .iter()
            .enumerate()
            .filter(|(_, (s, _))| *s == Sender::Client)
            .map(|(i, (_, p))| (i, *p))
            .collect::<Vec<_>>();
        let mut server_packets = packets
            .iter()
            .enumerate()
            .filter(|(_, (s, _))| *s == Sender::Server)
            .map(|(i, (_, p))| (i, *p))
            .collect::<Vec<_>>();

        let mut topo_edges = Vec::with_capacity(packets.len() * 3);

        {
            tracyrs::zone!("Stream::from", "topo edges");
            // linearize
            {
                tracyrs::zone!("Stream::from", "linearizeBySeq");
                topo_edges.extend(LinearizeBySeq::new(&client_packets));
                topo_edges.extend(LinearizeBySeq::new(&server_packets));
            }
            // ensure ack after seq
            {
                tracyrs::zone!("Stream::from", "linearizeBySeqAck");
                client_packets.sort_by_key(|(_, p)| p.tcp_header.sequence_no);
                server_packets.sort_by_key(|(_, p)| p.tcp_header.ack_no);
                topo_edges.extend(LinearizeBySeqAck::new(&client_packets, &server_packets));
                server_packets.sort_by_key(|(_, p)| p.tcp_header.sequence_no);
                client_packets.sort_by_key(|(_, p)| p.tcp_header.ack_no);
                topo_edges.extend(LinearizeBySeqAck::new(&server_packets, &client_packets));
            }

            {
                tracyrs::zone!("Stream::from", "sort/dedup");
                topo_edges.sort();
                topo_edges.dedup();
            }
        }

        let cyclic;
        if let Some(res) = topo_sort(&topo_edges, &packets) {
            // TODO: this takes a while for streams with >10k packets. look into wireshark's source maybe
            packets = res;
            cyclic = false;
        } else {
            cyclic = true;
        }

        // TODO: dedup acks that are directly followed by another PSH ack

        let mut missing_data = false;

        let mut segments = Vec::with_capacity(packets.len());
        let mut client_pos = 0;
        let mut server_pos = 0;
        for (sender, packet) in packets.into_iter() {
            let pos = match sender {
                Sender::Client => client_pos,
                Sender::Server => server_pos,
            };
            match sender {
                Sender::Client => client_pos += packet.data.len(),
                Sender::Server => server_pos += packet.data.len(),
            }

            if pos < packet.tcp_header.sequence_no as usize {
                missing_data = true;
            }

            match sender {
                Sender::Client => client_data.extend_from_slice(&packet.data),
                Sender::Server => server_data.extend_from_slice(&packet.data),
            }

            let th = &packet.tcp_header;
            let mut flags = 0;
            flags |= (th.flag_urg as u8) << 5;
            flags |= (th.flag_ack as u8) << 4;
            flags |= (th.flag_psh as u8) << 3;
            flags |= (th.flag_rst as u8) << 2;
            flags |= (th.flag_syn as u8) << 1;
            flags |= (th.flag_fin as u8) << 0;
            segments.push(crate::database::Segment {
                sender,
                start: pos,
                flags,
                seq: th.sequence_no,
                ack: th.ack_no,
                timestamp: packet
                    .timestamp
                    .map(|ts| {
                        ts.duration_since(std::time::SystemTime::UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as u64
                    })
                    .unwrap_or(0),
            });
        }

        StreamSegmentResult {
            segments,
            client_data,
            server_data,
            malformed,
            cyclic,
            missing_data,
        }
    }

    pub(crate) fn dummy() -> Self {
        let data_id = StreamPayloadID(0);
        let addr = (IpAddr::V6(std::net::Ipv6Addr::LOCALHOST), 0);
        Stream {
            id: StreamID::new(0),
            client: addr,
            server: addr,
            segments: Vec::new(),
            client_data_len: 0,
            server_data_len: 0,
            client_data_id: data_id,
            server_data_id: data_id,
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

#[derive(Debug)]
pub(crate) struct StreamSegmentResult {
    pub(crate) segments: Vec<Segment>,
    pub(crate) client_data: Vec<u8>,
    pub(crate) server_data: Vec<u8>,
    pub(crate) malformed: bool,
    pub(crate) cyclic: bool,
    pub(crate) missing_data: bool,
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

#[derive(Clone, Copy)]
pub(crate) struct StreamWithData<'a> {
    pub(crate) stream: &'a Stream,
    pub(crate) client_payload: &'a [u8],
    pub(crate) server_payload: &'a [u8],
}

pub(crate) enum StreamDataWrapper<'a> {
    Stream(&'a Stream),
    StreamWithDataRef(&'a Stream, Vec<u8>, Vec<u8>),
    StreamWithData(StreamWithData<'a>),
}

impl<'a> StreamDataWrapper<'a> {
    pub(crate) fn as_stream(&self) -> &Stream {
        match self {
            StreamDataWrapper::StreamWithData(swd) => &swd.stream,
            StreamDataWrapper::StreamWithDataRef(stream, _, _) => stream,
            StreamDataWrapper::Stream(stream) => stream,
        }
    }

    pub(crate) fn as_stream_with_data(&mut self, db: &Database) -> StreamWithData {
        match self {
            StreamDataWrapper::StreamWithData(swd) => *swd,
            StreamDataWrapper::StreamWithDataRef(stream, client_payload, server_payload) => {
                StreamWithData {
                    stream,
                    client_payload,
                    server_payload,
                }
            }
            StreamDataWrapper::Stream(stream) => {
                let client_data = db.datablob(stream.client_data_id).unwrap().to_vec(); // TODO
                let server_data = db.datablob(stream.client_data_id).unwrap().to_vec();
                *self = StreamDataWrapper::StreamWithDataRef(stream, client_data, server_data);
                self.as_stream_with_data(db)
            }
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StreamDetails {
    pub(crate) id: StreamID,
    pub(crate) client: (IpAddr, u16),
    pub(crate) server: (IpAddr, u16),
    pub(crate) tags: HashSet<TagID>,
    pub(crate) features: HashMap<TagID, f64>,
    pub(crate) segments: Vec<SegmentWithData>,
    pub(crate) client_data_len: u32,
    pub(crate) server_data_len: u32,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct SegmentWithData {
    pub(crate) sender: crate::database::Sender,
    #[serde(serialize_with = "crate::serde_aux::buffer_b64")]
    pub(crate) data: Vec<u8>,
    pub(crate) timestamp: u64,
    pub(crate) flags: u8,
    pub(crate) seq: u32,
    pub(crate) ack: u32,
}

struct LinearizeBySeq<'a> {
    seqs: &'a [(usize, &'a Packet)],
    left_idx: usize,
    right_idx: usize,
    buffer: VecDeque<(u32, u32)>,
}

impl<'a> LinearizeBySeq<'a> {
    fn new(seqs: &'a [(usize, &'a Packet)]) -> Self {
        LinearizeBySeq {
            buffer: VecDeque::new(),
            left_idx: 0,
            right_idx: 0,
            seqs,
        }
    }
}

impl<'a> Iterator for LinearizeBySeq<'a> {
    type Item = (u32, u32);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(res) = self.buffer.pop_front() {
                return Some(res);
            }
            self.right_idx += 1;
            if self.right_idx >= self.seqs.len() {
                return None;
            }
            let seq_no = self.seqs[self.right_idx].1.tcp_header.sequence_no;
            while self.left_idx < self.seqs.len()
                && self.seqs[self.left_idx].1.tcp_header.sequence_no < seq_no
            {
                let left_id = self.seqs[self.left_idx].0 as u32;
                for (right_id, right) in &self.seqs[self.right_idx..] {
                    if right.tcp_header.sequence_no != seq_no {
                        break;
                    }
                    self.buffer.push_back((left_id, *right_id as u32));
                }
                self.left_idx += 1;
            }
        }
    }
}

struct LinearizeBySeqAck<'a> {
    seqs: &'a [(usize, &'a Packet)],
    acks: &'a [(usize, &'a Packet)],
    left_idx: usize,
    right_idx: usize,
    buffer: VecDeque<(u32, u32)>,
}

impl<'a> LinearizeBySeqAck<'a> {
    fn new(seqs: &'a [(usize, &'a Packet)], acks: &'a [(usize, &'a Packet)]) -> Self {
        LinearizeBySeqAck {
            buffer: VecDeque::new(),
            left_idx: 0,
            right_idx: 0,
            seqs,
            acks,
        }
    }
}

impl<'a> Iterator for LinearizeBySeqAck<'a> {
    type Item = (u32, u32);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(res) = self.buffer.pop_front() {
                return Some(res);
            }
            if self.right_idx == self.acks.len() {
                return None;
            }
            if self.acks[self.right_idx].1.tcp_header.flag_ack {
                let ack = self.acks[self.right_idx].1.tcp_header.ack_no;
                while self.left_idx < self.seqs.len()
                    && self.seqs[self.left_idx].1.tcp_header.sequence_no < ack
                {
                    let left_id = self.seqs[self.left_idx].0 as u32;
                    for (right_id, right) in &self.acks[self.right_idx..] {
                        if right.tcp_header.ack_no != ack {
                            break;
                        }
                        self.buffer.push_back((left_id, *right_id as u32));
                    }
                    self.left_idx += 1;
                }
            }
            self.right_idx += 1;
        }
    }
}

struct TopoCmpHelper<'a>(u32, (Sender, &'a Packet));
impl<'a> PartialEq for TopoCmpHelper<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<'a> Eq for TopoCmpHelper<'a> {}
impl<'a> PartialOrd for TopoCmpHelper<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.1).1.timestamp.partial_cmp(&(other.1).1.timestamp)
    }
}
impl<'a> Ord for TopoCmpHelper<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.1).1.timestamp.cmp(&(other.1).1.timestamp)
    }
}

fn topo_sort<'a>(
    edges: &[(u32, u32)],
    packets: &[(Sender, &'a Packet)],
) -> Option<Vec<(Sender, &'a Packet)>> {
    tracyrs::zone!("topo_sort");
    let mut indeg: HashMap<u32, u32> = HashMap::new();
    let mut adj: HashMap<u32, Vec<u32>> = HashMap::new();
    for (i, _) in packets.iter().enumerate() {
        indeg.insert(i as u32, 0);
    }

    for (i, j) in edges.iter() {
        adj.entry(*i).or_default().push(*j);
        *indeg.entry(*j).or_default() += 1;
    }
    let mut pq = BinaryHeap::new();
    for (k, v) in indeg.iter() {
        if *v == 0 {
            pq.push(TopoCmpHelper(*k, packets[*k as usize]));
        }
    }

    let mut res = Vec::new();
    while let Some(TopoCmpHelper(current, payload)) = pq.pop() {
        res.push(payload);
        if let Some(adj_) = adj.get(&current) {
            for next in adj_ {
                *indeg.get_mut(next).unwrap() -= 1;
                if indeg[next] == 0 {
                    pq.push(TopoCmpHelper(*next, packets[*next as usize]));
                }
            }
        }
    }

    for (_, v) in indeg.iter() {
        if *v != 0 {
            return None;
        }
    }

    Some(res)
}

fn make_sequence_numbers_relative(
    a: &mut crate::reassembly::Stream,
    b: &mut crate::reassembly::Stream,
) -> Result<(), ()> {
    fn seq_start(seqs: &[Packet], acks: &[Packet]) -> Option<u32> {
        let mut buckets = [None, None, None];
        for seqno in seqs.iter().map(|p| p.tcp_header.sequence_no).chain(
            acks.iter()
                .filter(|p| p.tcp_header.flag_ack)
                .map(|p| p.tcp_header.ack_no),
        ) {
            let bucket_idx = (seqno >= 0x55555555) as usize + (seqno >= 0xaaaaaaaa) as usize;
            buckets[bucket_idx] = buckets[bucket_idx]
                .map(|seqno_: u32| seqno_.min(seqno))
                .or(Some(seqno));
        }
        match buckets {
            [None, None, None] => None,
            [Some(x), _, None] => Some(x),
            [None, Some(y), _] => Some(y),
            [_, None, Some(z)] => Some(z),
            _ => {
                eprintln!(
                    "sequence ids cross both boundaries: {:?} (#packets {})",
                    buckets,
                    seqs.len() + acks.len()
                );
                None
            }
        }
    }

    let mut malformed = false;

    let start_seq_a = seq_start(&a.packets, &b.packets).unwrap_or_else(|| {
        malformed = true;
        0
    });
    let start_seq_b = seq_start(&b.packets, &a.packets).unwrap_or_else(|| {
        malformed = true;
        0
    });

    for p in &mut a.packets {
        p.tcp_header.sequence_no = p.tcp_header.sequence_no.wrapping_sub(start_seq_a);
        if p.tcp_header.flag_ack {
            p.tcp_header.ack_no = p.tcp_header.ack_no.wrapping_sub(start_seq_b);
        }
    }
    for p in &mut b.packets {
        p.tcp_header.sequence_no = p.tcp_header.sequence_no.wrapping_sub(start_seq_b);
        if p.tcp_header.flag_ack {
            p.tcp_header.ack_no = p.tcp_header.ack_no.wrapping_sub(start_seq_a);
        }
    }

    if malformed {
        Err(())
    } else {
        Ok(())
    }
}
