use crate::database::{Database, StreamID, TagID, TagIndex};
use serde::{Serialize, Deserialize};

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

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Query {
    pub(crate) kind: QueryKind,
    pub(crate) limit: Option<u64>,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Cursor {
    pub(crate) query: Query,
    pub(crate) scan_offset: usize,
    pub(crate) scan_max: usize, // for progress indicators
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum QueryKind {
    All,
    Service(u16),
    Tagged(TagQuery),
    ServiceTagged(u16, TagQuery),
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum TagQuery {
    One(TagID),
    // All(Vec<TagID>),
    // Expr(Expr),
}

/*
pub(crate) enum Expr {
    Tag(TagID),
    Not(Box<Expr>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
}
*/

impl Query {
    pub(crate) fn execute(&self, db: &Database) -> Vec<StreamID> {
        match &self.kind {
            QueryKind::All => {
                todo!();
                // self.all(&*db.streams.read().unwrap())
            },
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
        }
    }

    fn all(&self, streams: &[StreamID]) -> Vec<StreamID> {
        todo!()
    }

    fn tagged(&self, streams: &TagIndex, tag_query: &TagQuery) -> Vec<StreamID> {
        todo!()
    }
}
