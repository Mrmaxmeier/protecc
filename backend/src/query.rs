use crate::database::{Database, Stream, TagID};
use serde::{Deserialize, Serialize};

use crate::incr_counter;

/// max entries returned by a single cursor step
const QUERY_RETURN_LIMIT: usize = 0x100;
/// max entries processed by a single cursor step
const QUERY_SCAN_LIMIT: usize = 0x10000;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Query {
    pub(crate) kind: QueryKind,
    pub(crate) filter: Option<QueryFilter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum IntExpr<T> {
    Eq(T),
    Le(T),
    Ge(T),
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct QueryFilter {
    service: Option<u16>,
    tag: Option<TagID>, // TODO: tag expression
    regex: Option<String>,
    length: Option<IntExpr<usize>>,
}

impl QueryFilter {
    fn matches(&self, stream: &Stream, db: &Database) -> bool {
        if let Some(service) = self.service {
            if service != stream.service() {
                return false;
            }
        }
        if let Some(tag) = self.tag {
            if !stream.tags.contains(&tag) {
                return false;
            }
        }
        if let Some(regex) = self.regex.as_ref() {
            let re = regex::bytes::Regex::new(&regex).unwrap(); // TODO: caching, error handling?
            let client_payload = db.datablob(stream.client_data_id);
            let server_payload = db.datablob(stream.server_data_id);

            if !(client_payload.map(|p| re.is_match(&p)).unwrap_or(false)
                || server_payload.map(|p| re.is_match(&p)).unwrap_or(false))
            {
                return false;
            }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum QueryKind {
    All,
    Service(u16),
    Tagged(TagID),
    ServiceTagged(u16, TagID),
}

impl Query {
    pub(crate) async fn into_cursor(self, db: &Database) -> Cursor {
        let scan_max = match self.kind {
            QueryKind::All => db.streams.read().await.len(),
            QueryKind::Service(port) => {
                let services = db.services.read().await;
                if let Some(service) = services.get(&port) {
                    service.read().await.streams.len()
                } else {
                    0
                }
            }
            QueryKind::Tagged(tag) => {
                let tag_index = db.tag_index.read().await;
                if let Some(service) = tag_index.tagged.get(&tag) {
                    service.len()
                } else {
                    0
                }
            }
            QueryKind::ServiceTagged(port, tag) => {
                let services = db.services.read().await;

                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(tag_index) = service.tag_index.as_ref() {
                        if let Some(service) = tag_index.tagged.get(&tag) {
                            service.len()
                        } else {
                            0
                        }
                    } else {
                        // note: this service doesn't provide an index for tags.
                        // fall back to linear scan over tagged values
                        assert!(self.filter.is_none(), "TODO");
                        return Cursor {
                            scan_max: service.streams.len(),
                            scan_offset: 0,
                            query: Query {
                                kind: QueryKind::Service(port),
                                filter: Some(QueryFilter {
                                    tag: Some(tag),
                                    ..QueryFilter::default()
                                }),
                            },
                        };
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
        match &self.query.kind {
            QueryKind::All => {
                let streams = db.streams.read().await;
                self.limit_and_filter(buffer, streams.iter().rev().skip(self.scan_offset), db)
            }
            QueryKind::Service(port) => {
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
            QueryKind::Tagged(tag_id) => {
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
            QueryKind::ServiceTagged(port, tag_id) => {
                let services = db.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(ref tag_index) = service.tag_index {
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
                    } else {
                        todo!()
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
            if self.query.filter.as_ref().map(|f| f.matches(stream, db)) != Some(false) {
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
