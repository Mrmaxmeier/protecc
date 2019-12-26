use std::cell::RefCell;

std::thread_local! {
    pub(crate) static TLS_COUNTERS: RefCell<Counters> = RefCell::new(Counters::default());
}

#[derive(Debug, Default)]
pub(crate) struct Counters {
    pub(crate) packets: u64,
    pub(crate) streams: u64,
    pub(crate) reassembly_errors: u64,
    pub(crate) packets_unhandled: u64,
    pub(crate) packets_malformed: u64,
    pub(crate) packets_without_stream: u64,
    pub(crate) packets_tcp: u64,
    pub(crate) streams_completed: u64,
    pub(crate) pcap_blocks: u64,
    pub(crate) pcaps_imported: u64,
    pub(crate) db_services: u64,
    pub(crate) db_stat_service_promotion: u64,
    pub(crate) query_rows_scanned: u64,
    pub(crate) query_rows_returned: u64,
}

pub(crate) fn _incr_counter_impl<F: Fn(&mut Counters)>(counter: &str, f: F) {
    TLS_COUNTERS.with(|c| f(&mut *c.borrow_mut()));
}

pub(crate) fn flush_tls() {
    // TODO
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
