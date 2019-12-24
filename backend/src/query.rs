use crate::database::{Database, Stream, StreamID, TagID, TagIndex};
use serde::{Deserialize, Serialize};

/*
TODO(impl):
- query all
- query service
- query tags
- query ip?
*/

/// max entries returned by a single cursor step
const QUERY_RETURN_LIMIT: usize = 0x100;
/// max entries processed by a single cursor step
const QUERY_SCAN_LIMIT: usize = 0x10000;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Query {
    pub(crate) kind: QueryKind,
    pub(crate) limit: Option<u64>,
    pub(crate) filter: Option<QueryFilter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct QueryFilter {
    // TODO: complex tag search
    // TODO: regex search
    // TODO: search by ip
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
    pub(crate) fn into_cursor(self, db: &Database) -> Cursor {
        let scan_max = match self.kind {
            QueryKind::All => db.streams.read().unwrap().len(),
            QueryKind::Service(port) => db
                .services
                .read()
                .unwrap()
                .get(&port)
                .map(|service| service.lock().unwrap().streams.len())
                .unwrap_or(0),
            QueryKind::Tagged(..) => todo!(),
            QueryKind::ServiceTagged(..) => todo!(),
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
    pub(crate) fn execute(&self, db: &Database, buffer: &mut Vec<Stream>) -> Cursor {
        let scan_idx = self.scan_max - self.scan_offset;
        match &self.query.kind {
            QueryKind::All => {
                let mut matched_streams = &db.streams.read().unwrap()[..scan_idx];
                if matched_streams.len() > QUERY_RETURN_LIMIT {
                    matched_streams =
                        &matched_streams[matched_streams.len() - QUERY_RETURN_LIMIT..];
                }
                let mut res = self.clone();
                res.scan_offset += matched_streams.len();
                for elem in matched_streams.iter().rev() {
                    buffer.push(elem.clone()); // TODO(perf): avoid this clone?
                }
                res
            }
            _ => todo!(),
            /*
            QueryKind::Service(port) => db
                .services
                .read()
                .unwrap()
                .get(&port)
                .map(|service| self.all(&service.lock().unwrap().streams))
                .unwrap_or(Vec::new()),
            QueryKind::Tagged(tag_query) => self.tagged(&*db.tag_index.lock().unwrap(), tag_query),
            QueryKind::ServiceTagged(port, tag_query) => db
                .services
                .read()
                .unwrap()
                .get(&port)
                .map(|service| self.tagged(&service.lock().unwrap().tag_index.as_ref().expect("no tag_index for query"), tag_query))
                .unwrap_or(Vec::new()),
                */
        }
    }
}
