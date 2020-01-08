use crate::database::{Database, Stream, StreamID};
use crate::query::QueryIndex;
use futures::FutureExt;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{broadcast, mpsc, watch, Mutex};
use tokio::time::{throttle, Throttle};

#[derive(Serialize, Deserialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WindowParameters {
    size: usize,
    attached: bool,
}

pub(crate) struct WindowHandle(mpsc::Sender<WindowParameters>);
impl WindowHandle {
    pub(crate) async fn update(&self, params: WindowParameters) {
        self.0.clone().send(params).await.unwrap();
    }
}

pub(crate) struct Window {
    db: Arc<Database>,
    index: QueryIndex,
    start_id: StreamID,
    end_id: StreamID,
    size: usize,
    latest_id_chan: Throttle<watch::Receiver<StreamID>>,
    params_rx: mpsc::Receiver<WindowParameters>,
}

impl Window {
    pub(crate) async fn new(index: &QueryIndex, db: Arc<Database>) -> (Self, Arc<WindowHandle>) {
        assert!(*index == QueryIndex::All, "TODOtodoTODO");
        let (params_tx, params_rx) = mpsc::channel(1);
        let mut latest_id_chan = db.stream_notification_rx.clone();
        let start_id = latest_id_chan.recv().await.unwrap();
        (
            Window {
                db,
                size: 0,
                start_id,
                end_id: start_id,
                latest_id_chan: throttle(std::time::Duration::MILLISECOND * 250, latest_id_chan),
                params_rx,
                index: *index,
            },
            Arc::new(WindowHandle(params_tx)),
        )
    }

    pub(crate) async fn params_update(&mut self, params: WindowParameters) -> WindowUpdate {
        assert!(params.attached, "TODO");
        if params.size > self.size {
            let old_size = self.size;
            self.size = params.size;
            let mut new = Vec::new();
            match self.index {
                QueryIndex::All => {
                    let streams = self.db.streams.read().await;
                    if self.start_id.idx() - self.end_id.idx() < params.size {
                        self.end_id = if self.end_id.idx() <= params.size {
                            StreamID::new(0)
                        } else {
                            StreamID::new(self.end_id.idx() - params.size)
                        }
                    }
                    let slice = &streams[self.end_id.idx()..self.start_id.idx()];
                    for elem in &slice[old_size..] {
                        new.push(elem.clone());
                    }
                }
                _ => todo!(),
            }
            dbg!(params, &new);
            return WindowUpdate {
                new,
                ..WindowUpdate::default()
            };
        }
        println!("windowUpdate default case rip");
        WindowUpdate::default()
    }
    pub(crate) async fn new_id(&mut self, id: StreamID) -> WindowUpdate {
        println!("window new id: {:?}", id);
        let prev_start = std::mem::replace(&mut self.start_id, id);
        let mut new = Vec::new();
        match self.index {
            QueryIndex::All => {
                let new_cnt = self.start_id.idx() - prev_start.idx();
                let cnt = new_cnt.min(self.size);
                let streams = self.db.streams.read().await;
                for elem in &streams[self.start_id.idx() - cnt..][..cnt] {
                    new.push(elem.clone());
                }
            }
            _ => todo!(),
        }
        WindowUpdate {
            new,
            ..WindowUpdate::default()
        }
    }

    pub(crate) async fn next_update(&mut self) -> WindowUpdate {
        futures::select! {
            params = self.params_rx.recv().fuse() => {
                self.params_update(params.unwrap()).await
            },
            new_id = self.latest_id_chan.next().fuse() => {
                self.new_id(new_id.unwrap()).await
            },
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WindowUpdate {
    new: Vec<Stream>,
    extended: Vec<Stream>,
    changed: Vec<Stream>,
    deleted: Vec<StreamID>, // for tags
}
