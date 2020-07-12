use crate::database::{Database, StreamID};
use crate::stream::LightweightStream;
use crate::stream::QueryIndex;
use futures::{FutureExt, SinkExt};
use serde::{Deserialize, Serialize};
use std::ops::Range;
use std::sync::Arc;
use tokio::stream::StreamExt;
use tokio::sync::{broadcast, mpsc, watch};
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

    range: Range<StreamID>,
    size: usize,
    attached: bool,

    limit_for_reattach: Option<StreamID>,

    update_id_chan: broadcast::Receiver<StreamID>,
    latest_id_chan: Throttle<watch::Receiver<StreamID>>,
    params_rx: mpsc::Receiver<WindowParameters>,
}

impl Window {
    pub(crate) async fn new(index: &QueryIndex, db: Arc<Database>) -> (Self, Arc<WindowHandle>) {
        let (params_tx, params_rx) = mpsc::channel(1);
        let update_id_chan = db.stream_update_tx.subscribe();
        let mut latest_id_chan = db.stream_notification_rx.clone();
        let stream_id_limit = latest_id_chan.recv().await.unwrap();
        let range = stream_id_limit..stream_id_limit;
        let window = Window {
            db,
            range,
            size: 0,
            attached: true,
            limit_for_reattach: None,
            latest_id_chan: throttle(std::time::Duration::from_millis(250), latest_id_chan),
            update_id_chan,
            params_rx,
            index: *index,
        };
        (window, Arc::new(WindowHandle(params_tx)))
    }

    pub(crate) async fn set_attached(&mut self, attached: bool) -> Option<WindowUpdate> {
        // tracyrs::zone!("window::set_attached");
        if self.attached == attached {
            return None;
        }
        if !attached {
            self.attached = false;
            None
        } else {
            self.attached = true;
            if let Some(start_id) = self.limit_for_reattach.take() {
                self.new_id(start_id).await
            } else {
                None
            }
        }
    }

    pub(crate) async fn set_size(&mut self, size: usize) -> Option<WindowUpdate> {
        // tracyrs::zone!("window::set_size");
        let old_size = self.size;
        self.size = size;
        let mut new = Vec::new();
        if size > old_size {
            let streams = self.db.streams.read().await;
            let ret_cnt = size - old_size;
            let mut range_start = self
                .db
                .with_index_iter(self.index, ..self.range.start, |iter| {
                    let mut range_start = StreamID::new(0);
                    for stream_id in iter.rev().take(ret_cnt) {
                        new.push(streams[stream_id.idx()].as_lightweight());
                        range_start = stream_id;
                    }
                    range_start
                })
                .await;
            if new.len() != ret_cnt {
                range_start = StreamID::new(0);
            }
            self.range = range_start..self.range.end;
            crate::counters::update_counters(|c| c.window_sent += new.len() as u64);
            return Some(WindowUpdate {
                new,
                ..WindowUpdate::default()
            });
        }
        None
    }

    pub(crate) async fn new_id(&mut self, id: StreamID) -> Option<WindowUpdate> {
        // tracyrs::zone!("window::new_id");
        if !self.attached {
            self.limit_for_reattach = Some(id);
            return None;
        }

        // bump the range iff the new id matches our index
        let id = self
            .db
            .with_index_iter(self.index, id..=id, |iter| {
                let mut ret = id;
                for id in iter {
                    ret = id.next();
                }
                ret
            })
            .await;

        let prev_start = self.range.start;
        let old_range = std::mem::replace(&mut self.range, prev_start..id);

        let mut new = Vec::new();
        let streams = self.db.streams.read().await;
        self.db
            .with_index_iter(self.index, old_range.end..self.range.end, |iter| {
                for stream_id in iter.rev().take(self.size) {
                    new.push(streams[stream_id.idx()].as_lightweight());
                }
            })
            .await;

        crate::counters::update_counters(|c| c.window_sent += new.len() as u64);
        Some(WindowUpdate {
            new,
            ..WindowUpdate::default()
        })
    }

    pub(crate) async fn changed_id(&mut self, id: StreamID) -> Option<WindowUpdate> {
        // tracyrs::zone!("window::changed_id");
        if !self.range.contains(&id) {
            return None;
        }

        let was_deleted = self
            .db
            .with_index_iter(self.index, id..=id, |iter| {
                for _ in iter {
                    return false;
                }
                true
            })
            .await;

        if was_deleted {
            self.size -= 1;
            let mut update = self.set_size(self.size + 1).await.unwrap_or_default();
            update.deleted.push(id);
            crate::incr_counter!(window_sent);
            Some(update)
        } else {
            let streams = self.db.streams.read().await;
            crate::incr_counter!(window_sent);
            Some(WindowUpdate {
                changed: vec![streams[id.idx()].as_lightweight()],
                ..WindowUpdate::default()
            })
        }
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
                    if attached != self.attached {
                        if let Some(update) = self.set_attached(attached).await {
                            resp_tx.send(resp_frame(update)).await.unwrap();
                        }
                    }
                    if size != self.size {
                        if let Some(update) = self.set_size(size).await {
                            resp_tx.send(resp_frame(update)).await.unwrap();
                        }
                    }
                },
                new_id = self.latest_id_chan.next().fuse() => {
                    if let Some(update) = self.new_id(new_id.unwrap()).await {
                        resp_tx.send(resp_frame(update)).await.unwrap();
                    }
                },
                changed_id = self.update_id_chan.next().fuse() => {
                    if let Some(update) = self.changed_id(changed_id.unwrap().unwrap()).await {
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
    changed: Vec<LightweightStream>,
    deleted: Vec<StreamID>, // for tags
}
