use crate::database::{Database, StreamID};
use crate::query::QueryIndex;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{broadcast, mpsc, watch, RwLock};

pub(crate) struct Window {
    index: QueryIndex,
    start_id: StreamID,
    end_id: StreamID,
    size: usize,
    latest_id_chan: watch::Receiver<StreamID>,
    req_chan_tx: mpsc::Sender<()>,
}

impl Window {
    pub(crate) async fn new(index: QueryIndex, db: &Database) -> Self {
        let (req_chan_tx, req_chan_rx) = mpsc::channel(1);
        let mut latest_id_chan = db.stream_notification_rx.clone();
        let start_id = latest_id_chan.recv().await.unwrap();
        Window {
            size: 0,
            start_id,
            end_id: start_id,
            latest_id_chan,
            req_chan_tx,
            index,
        }
    }
}
