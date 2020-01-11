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

    stream_id_limit: StreamID,
    size: usize,
    attached: bool,

    limit_for_reattach: Option<StreamID>,

    start_idx: usize,
    end_idx: usize,

    latest_id_chan: Throttle<watch::Receiver<StreamID>>,
    params_rx: mpsc::Receiver<WindowParameters>,
}

impl Window {
    pub(crate) async fn new(index: &QueryIndex, db: Arc<Database>) -> (Self, Arc<WindowHandle>) {
        assert!(*index == QueryIndex::All, "TODOtodoTODO");
        let (params_tx, params_rx) = mpsc::channel(1);
        let mut latest_id_chan = db.stream_notification_rx.clone();
        let stream_id_limit = latest_id_chan.recv().await.unwrap();
        let mut window = Window {
            db,
            size: 0,
            stream_id_limit,
            start_idx: 0,
            end_idx: 0,
            attached: true,
            limit_for_reattach: None,
            latest_id_chan: throttle(std::time::Duration::MILLISECOND * 250, latest_id_chan),
            params_rx,
            index: *index,
        };
        window.fixup_start_end_idx().await;
        (window, Arc::new(WindowHandle(params_tx)))
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
            if let Some(start_id) = self.limit_for_reattach.take() {
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
            self.end_idx = self.start_idx.checked_sub(self.size).unwrap_or(0);
            let mut extended = Vec::new();
            match self.index {
                QueryIndex::All => {
                    let streams = self.db.streams.read().await;
                    let slice = &streams[self.end_idx..self.start_idx];
                    for elem in &slice[..(size - old_size)] {
                        extended.push(elem.as_lightweight());
                    }
                }
                _ => {
                    let streams = self.db.streams.read().await;
                    self.with_streamid_slice(|stream_ids| {
                        for elem in &stream_ids[..(size - old_size)] {
                            extended.push(streams[elem.idx()].as_lightweight());
                        }
                    })
                    .await;
                }
            }
            return Some(WindowUpdate {
                extended,
                ..WindowUpdate::default()
            });
        }
        None
    }

    pub(crate) fn binsearch(slice: &[StreamID], id: StreamID) -> usize {
        for (i, elem) in slice.iter().rev().enumerate().take(16) {
            if elem.idx() <= id.idx() {
                return slice.len() - i - 1;
            }
        }

        let mut l = 0;
        let mut r = slice.len() - 1;
        while l <= r {
            let m = (l + r) / 2;
            if slice[m].idx() > id.idx() {
                r = m + 1;
            } else if slice[m].idx() < id.idx() {
                l = m - 1;
            } else {
                return m;
            }
        }
        l
    }

    pub(crate) async fn new_id(&mut self, id: StreamID) -> Option<WindowUpdate> {
        if !self.attached {
            self.limit_for_reattach = Some(id);
            return None;
        }

        self.stream_id_limit = id;
        let mut new = Vec::new();
        match self.index {
            QueryIndex::All => {
                let new_cnt = id.idx() - self.start_idx;
                self.start_idx = id.idx();
                self.end_idx = self.end_idx + new_cnt;
                let cnt = new_cnt.min(self.size);
                let streams = self.db.streams.read().await;
                for elem in &streams[self.start_idx - cnt..][..cnt] {
                    new.push(elem.as_lightweight());
                }
            }
            _ => {
                let prev_idx = self.start_idx;
                let db = self.db.clone();
                let streams = db.streams.read().await;
                let (start_idx, end_idx) = self
                    .with_streamid_slice(|stream_ids| {
                        let start_idx = Self::binsearch(stream_ids, id);
                        let end_idx = self.start_idx.checked_sub(self.size).unwrap_or(0);
                        let send_cnt = start_idx - prev_idx;
                        for elem in &stream_ids[end_idx..start_idx][..send_cnt] {
                            new.push(streams[elem.idx()].as_lightweight());
                        }
                        (start_idx, end_idx)
                    })
                    .await;
                self.start_idx = start_idx;
                self.end_idx = end_idx;
            }
        }
        Some(WindowUpdate {
            new,
            ..WindowUpdate::default()
        })
    }

    pub(crate) async fn fixup_start_end_idx(&mut self) {
        if let QueryIndex::All = self.index {
            self.start_idx = self.stream_id_limit.idx();
            self.end_idx = self.start_idx;
            return;
        }
        let stream_id = self.stream_id_limit;
        let (idx, _sid) = self
            .with_streamid_slice(|stream_ids| {
                for (i, elem) in stream_ids.iter().rev().enumerate() {
                    if elem.idx() <= stream_id.idx() {
                        return (stream_ids.len() - i - 1, *elem);
                    }
                }
                (0usize, StreamID::new(0))
            })
            .await;
        self.start_idx = idx;
        self.end_idx = idx;
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
