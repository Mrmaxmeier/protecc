use derive_more::{Add, AddAssign};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::sync::Mutex;

std::thread_local! {
    pub(crate) static TLS_COUNTERS: RefCell<Counters> = RefCell::new(Counters::default());
}

lazy_static! {
    pub(crate) static ref COUNTERS: Mutex<Counters> = Mutex::new(Counters::default());
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

pub(crate) fn _incr_counter_impl<F: Fn(&mut Counters)>(_counter: &str, f: F) {
    TLS_COUNTERS.with(|c| f(&mut *c.borrow_mut()));
    flush_tls(); // TODO(perf)
}

pub(crate) fn flush_tls() {
    let mut counters = COUNTERS.lock().unwrap();
    TLS_COUNTERS.with(|c| {
        let tls_counters = std::mem::replace(&mut *c.borrow_mut(), Counters::default());
        *counters += tls_counters;
    });
}

#[macro_export]
macro_rules! incr_counter {
    ($x:ident) => {{
        crate::counters::_incr_counter_impl(stringify!($x), |c| {
            c.$x += 1;
            if c.$x.is_power_of_two() {
                println!("counter {}: {}", stringify!($x), c.$x);
            }
        });
        ()
    }};
    ($x:ident, $msg:tt) => {
        println!($msg);
        incr_counter!($x);
    };
}
