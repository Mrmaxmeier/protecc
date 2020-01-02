use derive_more::{Add, AddAssign};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use tokio::stream::StreamExt;
use tokio::sync::{mpsc, watch};
use futures::FutureExt;

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


#[derive(Debug, Default, Clone, Add, AddAssign, Serialize, Deserialize)] // Note: We're not using Deserialize
pub(crate) struct Counters {
    pub(crate) packets: u64,
    pub(crate) streams: u64,
    pub(crate) reassembly_errors: u64,
    pub(crate) packets_unhandled: u64,
    pub(crate) packets_malformed: u64,
    pub(crate) packets_without_stream: u64,
    pub(crate) packets_tcp: u64,
    pub(crate) streams_completed: u64,
    pub(crate) streams_timeout_expired: u64,
    pub(crate) pcap_blocks: u64,
    pub(crate) pcaps_imported: u64,
    pub(crate) db_services: u64,
    pub(crate) db_stat_service_promotion: u64,
    pub(crate) query_rows_scanned: u64,
    pub(crate) query_rows_returned: u64,
    pub(crate) ws_connections: u64,
    pub(crate) ws_rx: u64,
    pub(crate) ws_tx: u64,
}

fn aggregate_counters() -> (
    mpsc::UnboundedSender<CountersCell>,
    watch::Receiver<Counters>,
) {
    let (agg_tx, mut agg_rx) = mpsc::unbounded_channel::<CountersCell>();
    let (counters_tx, counters_rx) = watch::channel(Counters::default());
    let mut delay_queue = tokio::time::DelayQueue::new();

    let mut counters = Counters::default();
    tokio::spawn((async move || {
    loop {
        futures::select! {
            elem = agg_rx.recv().fuse() => {
                println!("counter update queued");
                delay_queue.insert(elem.unwrap(), std::time::Duration::SECOND);
            },
            elem = delay_queue.next().fuse() => {
                if elem.is_some() {
                    let elem = elem.expect("end of delay queue!?").expect("delay queue time error").into_inner();
                    let mut delta = elem.lock().unwrap();
                    let delta = std::mem::replace(&mut *delta, None).expect("double-collect of counters");
                    // TODO: check_for_pow_2(&counters, &delta);
                    counters += delta;
                    counters_tx.broadcast(counters.clone()).unwrap();
                } else {
                    // TODO: wtf is going on here. why does delay_queue.next().fuse return None randomly
                }
            },
        };
    }
})());

/*
    tokio::spawn((async move || {
        while let Some(elem) = agg_rx.recv().await {
            delay_queue.insert(elem, std::time::Duration::SECOND);
        }
    })());

    tokio::spawn((async move || {
        let mut counters = Counters::default();
        while let Some(elem) = delay_queue.next().await {
            let elem = elem.unwrap().into_inner();
            let mut delta = elem.lock().unwrap();
            let delta = std::mem::replace(&mut *delta, None).expect("double-collect of counters");
            // TODO: check_for_pow_2(&counters, &delta);
            counters += delta;
            counters_tx.broadcast(counters.clone()).unwrap();
        }
    })());
*/

    (agg_tx, counters_rx)
}

pub(crate) fn _incr_counter_impl<F: Fn(&mut Counters)>(_counter: &str, f: F) {
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
        crate::counters::_incr_counter_impl(stringify!($x), |c| {
            c.$x += 1;
            /*
            if c.$x.is_power_of_two() {
                println!("counter {}: {}", stringify!($x), c.$x);
            }
            */
        });
        ()
    }};
    ($x:ident, $msg:tt) => {
        println!($msg);
        incr_counter!($x);
    };
}
