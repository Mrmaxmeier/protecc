use crate::database::{Database, StreamID, TagID, TagIndex};

pub(crate) struct Query {
    pub(crate) kind: QueryKind,
    pub(crate) limit: Option<u64>,
}

pub(crate) enum QueryKind {
    All,
    Service(u16),
    Tagged(TagQuery),
    ServiceTagged(u16, TagQuery),
}

pub(crate) enum TagQuery {
    One(TagID),
    All(Vec<TagID>),
    Expr(Expr),
}

pub(crate) enum Expr {
    Tag(TagID),
    Not(Box<Expr>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
}

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
