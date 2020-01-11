use derive_more::{Add, AddAssign};
use futures::FutureExt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch};
use tokio::time::DelayQueue;

type CountersCell = Arc<Mutex<Option<Counters>>>;

std::thread_local! {
    pub(crate) static TLS_COUNTERS: CountersCell = Arc::default();
}

lazy_static::lazy_static! {
    static ref GLOBAL_COUNTERS_CHANS: (mpsc::UnboundedSender<CountersCell>, watch::Receiver<Counters>) = aggregate_counters();
}

pub(crate) fn subscribe() -> watch::Receiver<Counters> {
    GLOBAL_COUNTERS_CHANS.1.clone()
}

#[derive(Debug, Default, Clone, Add, AddAssign, Serialize, Deserialize)]
pub(crate) struct Counters {
    pub(crate) packets_unhandled: u64,
    pub(crate) packets_malformed: u64,
    pub(crate) packets_without_stream: u64,
    pub(crate) packets_tcp: u64,
    pub(crate) packet_bytes: u64,
    pub(crate) pcap_processing_milliseconds: u64,
    pub(crate) streams_completed: u64,
    pub(crate) streams_timeout_expired: u64,
    pub(crate) streams_without_syn: u64,
    pub(crate) pcap_blocks: u64,
    pub(crate) pcaps_imported: u64,
    pub(crate) db_services: u64,
    pub(crate) db_streams_rss: u64,
    pub(crate) query_rows_scanned: u64,
    pub(crate) query_rows_returned: u64,
    pub(crate) ws_connections: u64,
    pub(crate) ws_rx: u64,
    pub(crate) ws_tx: u64,
}

#[derive(Debug, Serialize, Deserialize)]
#[repr(transparent)]
pub(crate) struct CountersAsMapHack(HashMap<String, u64>);
impl CountersAsMapHack {
    fn from_counters(counters: &Counters) -> Self {
        let val = serde_json::to_value(counters).unwrap();
        serde_json::from_value(val).unwrap()
    }
    /*
        fn into_counters(&self) -> Counters {
            let val = serde_json::to_value(self).unwrap();
            serde_json::from_value(val).unwrap()
        }
    */
}

impl Counters {
    pub(crate) fn as_hashmap(&self) -> HashMap<String, u64> {
        CountersAsMapHack::from_counters(self).0
    }
}

fn aggregate_counters() -> (
    mpsc::UnboundedSender<CountersCell>,
    watch::Receiver<Counters>,
) {
    let (agg_tx, mut agg_rx) = mpsc::unbounded_channel::<CountersCell>();
    let (counters_tx, counters_rx) = watch::channel(Counters::default());
    let mut delay_queue = DelayQueue::new();

    let mut counters = Counters::default();
    tokio::spawn(async move {
        async fn delay_queue_next(dq: &mut DelayQueue<CountersCell>) -> CountersCell {
            // for some stupid reason, delayqueue.next() returns with Ready(None) even though it's empty.
            loop {
                let elem = dq.next().await;
                match elem {
                    Some(x) => return x.expect("tokio time error").into_inner(),
                    None => futures::pending!(),
                }
            }
        }
        loop {
            futures::select! {
                elem = agg_rx.recv().fuse() => {
                    delay_queue.insert(elem.unwrap(), std::time::Duration::SECOND);
                },
                elem = delay_queue_next(&mut delay_queue).fuse() => {
                    let mut delta = elem.lock().unwrap();
                    let delta = std::mem::replace(&mut *delta, None).expect("double-collect of counters");
                    let before = CountersAsMapHack::from_counters(&counters);
                    counters += delta;
                    let after = CountersAsMapHack::from_counters(&counters);
                    for (k, a) in before.0.iter() {
                        let b = after.0.get(k).unwrap();
                        if check_for_pow2(*a, *b) {
                            println!("{}: {} (>= {:#x})", k, b, (a+1).checked_next_power_of_two().unwrap());
                        }
                    }
                    counters_tx.broadcast(counters.clone()).unwrap();
                },
            };
        }
    });

    (agg_tx, counters_rx)
}

fn check_for_pow2(from: u64, to: u64) -> bool {
    (from + 1)
        .checked_next_power_of_two()
        .map(|x| to >= x)
        .unwrap_or(false)
}

pub(crate) fn update_counters<F: Fn(&mut Counters)>(f: F) {
    TLS_COUNTERS.with(|c| {
        let mut ctrs = c.lock().unwrap();
        match ctrs.as_mut() {
            Some(ctrs) => f(ctrs),
            None => {
                let mut new_ctrs = Counters::default();
                f(&mut new_ctrs);
                *ctrs = Some(new_ctrs);
                GLOBAL_COUNTERS_CHANS.0.clone().send(c.clone()).unwrap();
            }
        }
    });
}

#[macro_export]
macro_rules! incr_counter {
    ($x:ident) => {{
        crate::counters::update_counters(|c| {
            c.$x += 1;
        });
        ()
    }};
    ($x:ident, $msg:tt) => {
        println!($msg);
        incr_counter!($x);
    };
}
