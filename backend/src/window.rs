use crate::database::{Database, StreamID};
use crate::query::QueryIndex;
use crate::stream::LightweightStream;
use futures::{FutureExt, SinkExt};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch};
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
    attached: bool,

    start_id_for_reattach: Option<StreamID>,

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
                attached: true,
                start_id_for_reattach: None,
                latest_id_chan: throttle(std::time::Duration::MILLISECOND * 250, latest_id_chan),
                params_rx,
                index: *index,
            },
            Arc::new(WindowHandle(params_tx)),
        )
    }

    pub(crate) async fn set_attached(&mut self, attached: bool) -> Option<WindowUpdate> {
        if self.attached == attached {
            return None;
        }
        if !attached {
            self.attached = false;
            None
        } else {
            self.attached = true;
            if let Some(start_id) = self.start_id_for_reattach.take() {
                self.new_id(start_id).await
            } else {
                None
            }
        }
    }

    pub(crate) async fn with_streamid_slice<R, F: FnOnce(&[StreamID]) -> R>(&self, f: F) -> R {
        match self.index {
            QueryIndex::All => unreachable!(),
            QueryIndex::Service(port) => {
                let services = self.db.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    f(&service.streams)
                } else {
                    f(&[])
                }
            }
            QueryIndex::ServiceTagged(port, tag) => {
                let services = self.db.services.read().await;
                if let Some(service) = services.get(&port) {
                    let service = service.read().await;
                    if let Some(streams) = service.tag_index.tagged.get(&tag) {
                        f(&streams)
                    } else {
                        f(&[])
                    }
                } else {
                    f(&[])
                }
            }
            QueryIndex::Tagged(tag) => {
                let tag_index = self.db.tag_index.read().await;
                if let Some(streams) = tag_index.tagged.get(&tag) {
                    f(&streams)
                } else {
                    f(&[])
                }
            }
        }
    }

    pub(crate) async fn set_size(&mut self, size: usize) -> Option<WindowUpdate> {
        assert!(size >= self.size, "TODO: handle window downsizing");
        if size > self.size {
            let old_size = self.size;
            self.size = size;
            let mut extended = Vec::new();
            match self.index {
                QueryIndex::All => {
                    let streams = self.db.streams.read().await;
                    if self.start_id.idx() - self.end_id.idx() < self.size {
                        self.end_id = if self.end_id.idx() <= self.size {
                            StreamID::new(0)
                        } else {
                            StreamID::new(self.start_id.idx() - self.size)
                        }
                    }
                    let slice = &streams[self.end_id.idx()..self.start_id.idx()];
                    for elem in &slice[..(size - old_size)] {
                        extended.push(elem.as_lightweight());
                    }
                }
                _ => todo!(),
            }
            return Some(WindowUpdate {
                extended,
                ..WindowUpdate::default()
            });
        }
        None
    }
    pub(crate) async fn new_id(&mut self, id: StreamID) -> Option<WindowUpdate> {
        if !self.attached {
            self.start_id_for_reattach = Some(id);
            return None;
        }

        let prev_start = std::mem::replace(&mut self.start_id, id);
        let mut new = Vec::new();
        match self.index {
            QueryIndex::All => {
                let new_cnt = self.start_id.idx() - prev_start.idx();
                self.end_id = StreamID::new(self.end_id.idx() + new_cnt);
                let cnt = new_cnt.min(self.size);
                let streams = self.db.streams.read().await;
                for elem in &streams[self.start_id.idx() - cnt..][..cnt] {
                    new.push(elem.as_lightweight());
                }
            }
            _ => todo!(),
        }
        Some(WindowUpdate {
            new,
            ..WindowUpdate::default()
        })
    }

    pub(crate) async fn stream_results_to(
        &mut self,
        req_id: u64,
        mut resp_tx: futures::channel::mpsc::Sender<crate::wsserver::RespFrame>,
    ) {
        let resp_frame = move |window_update| {
            let payload = crate::wsserver::ResponsePayload::WindowUpdate(window_update);
            crate::wsserver::RespFrame {
                id: req_id,
                payload,
            }
        };

        loop {
            futures::select! {
                params = self.params_rx.recv().fuse() => {
                    let WindowParameters { size, attached } = params.unwrap();
                    if let Some(update) = self.set_attached(attached).await {
                        resp_tx.send(resp_frame(update)).await.unwrap();
                    }
                    if let Some(update) = self.set_size(size).await {
                        resp_tx.send(resp_frame(update)).await.unwrap();
                    }
                },
                new_id = self.latest_id_chan.next().fuse() => {
                    if let Some(update) = self.new_id(new_id.unwrap()).await {
                        resp_tx.send(resp_frame(update)).await.unwrap();
                    }
                },
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WindowUpdate {
    new: Vec<LightweightStream>,
    extended: Vec<LightweightStream>,
    changed: Vec<LightweightStream>,
    deleted: Vec<StreamID>, // for tags
}
