use crate::stream::Stream;
use crate::{
    database::{Database, TagID},
    stream::QueryIndex,
};
use regex::bytes::Regex;
use serde::{Deserialize, Serialize};

use crate::incr_counter;

/// max entries returned by a single cursor step
const QUERY_RETURN_LIMIT: usize = 0x100;
/// max entries processed by a single cursor step
const QUERY_SCAN_LIMIT: usize = 0x10000;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Query {
    pub(crate) index: QueryIndex,
    pub(crate) filter: Option<QueryFilter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum IntExpr<T> {
    Eq(T),
    Le(T),
    Ge(T),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum TrueFalse<T> {
    True(T),
    False(T),
}
// Expression in CNF
#[derive(Debug, Clone, Serialize, Deserialize)]
struct BoolExpr<T>(Vec<Vec<TrueFalse<T>>>);

impl<T: PartialEq + Eq> BoolExpr<T> {
    fn matches(&self, vars: &[T]) -> bool {
        self.0.iter().all(|clauses| {
            clauses.iter().any(|elem| match elem {
                TrueFalse::True(e) => vars.contains(e),
                TrueFalse::False(e) => !vars.contains(e),
            })
        })
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct QueryFilter {
    service: Option<u16>,
    tag: Option<TagID>,
    tags_cnf: Option<BoolExpr<TagID>>,
    regex: Option<Regex>,
    length: Option<IntExpr<usize>>,
}

impl QueryFilter {
    pub(crate) fn matches(&self, swd: &mut StreamDataWrapper, db: &Database) -> bool {
        if let Some(service) = self.service {
            if service != swd.as_stream().service() {
                return false;
            }
        }
        if let Some(tag) = self.tag {
            if !swd.as_stream().tags.contains(&tag) {
                return false;
            }
        }
        if let Some(regex) = self.regex.as_ref() {
            let swd = swd.as_stream_with_data(db);
            if !(regex.is_match(&*swd.client_payload) || regex.is_match(&*swd.server_payload)) {
                return false;
            }
        }
        if let Some(tags_cnf) = self.tags_cnf.as_ref() {
            let tags = swd.as_stream().tags.iter().cloned().collect::<Vec<_>>();
            if !tags_cnf.matches(&tags) {
                return false;
            }
        }
        if let Some(_) = self.length {
            unimplemented!();
        }
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Cursor {
    pub(crate) query: Query,
    pub(crate) scan_offset: usize, // idx initially eq scan_max
    pub(crate) scan_max: usize,    // for progress indicators
}

impl Query {
    pub(crate) async fn into_cursor(self, db: &Database) -> Cursor {
        let scan_max = match self.index {
            QueryIndex::All => db.streams.read().await.len(),
            QueryIndex::Service(port) => {
                let services = db.services.read().await;
                if let Some(service) = services.get(&port) {
                    service.read().await.streams.len()
                } else {
                    0
                }
            }
            QueryIndex::Tagged(tag) => {
                let tag_index = db.tag_index.read().await;
                if let Some(service) = tag_index.tagged.get(&tag) {
                    service.len()
                } else {
                    0
                }
            }
            QueryIndex::ServiceTagged(port, tag) => {
                let services = db.services.read().await;

                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(service) = service.tag_index.tagged.get(&tag) {
                        service.len()
                    } else {
                        0
                    }
                } else {
                    0
                }
            }
        };
        Cursor {
            query: self,
            scan_offset: 0,
            scan_max,
        }
    }
}

impl Cursor {
    pub(crate) fn has_next(&self) -> bool {
        self.scan_offset != self.scan_max
    }
    pub(crate) async fn execute(&self, db: &Database, buffer: &mut Vec<Stream>) -> Cursor {
        match &self.query.index {
            QueryIndex::All => {
                let streams = db.streams.read().await;
                self.limit_and_filter(buffer, streams.iter().rev().skip(self.scan_offset), db)
            }
            QueryIndex::Service(port) => {
                let services = db.services.read().await;
                if let Some(service) = services.get(&port) {
                    let all_streams = db.streams.read().await;
                    let scan_streams = &service.read().await.streams;
                    self.limit_and_filter(
                        buffer,
                        scan_streams
                            .iter()
                            .rev()
                            .skip(self.scan_offset)
                            .map(|stream_id| &all_streams[stream_id.idx()]),
                        db,
                    )
                } else {
                    self.clone()
                }
            }
            QueryIndex::Tagged(tag_id) => {
                let tag_index = db.tag_index.read().await;
                if let Some(scan_streams) = tag_index.tagged.get(tag_id) {
                    let all_streams = db.streams.read().await;
                    self.limit_and_filter(
                        buffer,
                        scan_streams
                            .iter()
                            .rev()
                            .skip(self.scan_offset)
                            .map(|stream_id| &all_streams[stream_id.idx()]),
                        db,
                    )
                } else {
                    self.clone()
                }
            }
            QueryIndex::ServiceTagged(port, tag_id) => {
                let services = db.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(scan_streams) = service.tag_index.tagged.get(tag_id) {
                        let all_streams = db.streams.read().await;
                        self.limit_and_filter(
                            buffer,
                            scan_streams
                                .iter()
                                .rev()
                                .skip(self.scan_offset)
                                .map(|stream_id| &all_streams[stream_id.idx()]),
                            db,
                        )
                    } else {
                        self.clone()
                    }
                } else {
                    self.clone()
                }
            }
        }
    }

    fn limit_and_filter<'a, I>(
        &'a self,
        buffer: &'a mut Vec<Stream>,
        streams: I,
        db: &Database,
    ) -> Cursor
    where
        I: Iterator<Item = &'a Stream>,
    {
        let mut returned_cnt = 0;
        let mut processed_cnt = 0;
        for stream in streams {
            incr_counter!(query_rows_scanned);
            if self.query.filter.as_ref().map(|f| {
                f.matches(
                    &mut StreamDataWrapper::Stream(std::sync::Arc::new(stream.clone())),
                    db,
                )
            }) != Some(false)
            {
                incr_counter!(query_rows_returned);
                buffer.push(stream.clone());
                returned_cnt += 1;
            }
            processed_cnt += 1;
            if returned_cnt >= QUERY_RETURN_LIMIT || processed_cnt >= QUERY_SCAN_LIMIT {
                break;
            }
        }
        let mut res = self.clone();
        res.scan_offset += processed_cnt;
        res
    }
}
