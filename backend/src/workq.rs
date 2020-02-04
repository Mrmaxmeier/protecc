use std::collections::VecDeque;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use tokio::sync::{Mutex, Semaphore};

/// Futures-aware mpmc channel with tracy instrumentation.
#[derive(Debug)]
pub(crate) struct WorkQ<T> {
    data: Mutex<VecDeque<T>>,
    free: Semaphore,
    ready: Semaphore,
    size: usize,
    debug_name: Option<&'static [u8]>,
    blocking_push_count: AtomicU32,
    blocking_pop_count: AtomicU32,
}

impl<T> WorkQ<T> {
    pub(crate) fn new(size: usize, debug_name: Option<&'static [u8]>) -> Arc<Self> {
        Arc::new(WorkQ {
            data: Mutex::new(VecDeque::with_capacity(size)),
            free: Semaphore::new(size),
            ready: Semaphore::new(0),
            blocking_pop_count: AtomicU32::new(0),
            blocking_push_count: AtomicU32::new(0),
            size,
            debug_name,
        })
    }

    pub(crate) async fn push(&self, elem: T) {
        if let Ok(permit) = self.free.try_acquire() {
            permit.forget();
        } else {
            self.blocking_push_count.fetch_add(1, Ordering::Relaxed);
            tracyrs::message!("blocking WorkQ::push");
            self.free.acquire().await.forget();
            tracyrs::message!("~blocking WorkQ::push");
        }
        {
            let mut data = self.data.lock().await;
            data.push_back(elem);
            if let Some(debug_name) = self.debug_name {
                tracyrs::emit_plot(debug_name, data.len() as f64);
            }
        }
        self.ready.add_permits(1);
    }

    pub(crate) async fn pop(&self) -> T {
        self.ready.acquire().await.forget();
        let res;
        {
            let mut data = self.data.lock().await;
            res = data.pop_front().expect("invalid WorkQ state");
            if let Some(debug_name) = self.debug_name {
                tracyrs::emit_plot(debug_name, data.len() as f64);
            }
        }
        self.free.add_permits(1);
        res
    }

    pub(crate) async fn pop_batch(&self, buffer: &mut Vec<T>) {
        if let Ok(permit) = self.ready.try_acquire() {
            permit.forget();
        } else {
            self.blocking_pop_count.fetch_add(1, Ordering::Relaxed);
            tracyrs::message!("blocking WorkQ::pop_batch");
            self.ready.acquire().await.forget();
            tracyrs::message!("~blocking WorkQ::pop_batch");
        }
        let mut permits = 1;
        while let Ok(permit) = self.ready.try_acquire() {
            permits += 1;
            permit.forget();
            if permits >= self.size / 3 {
                break;
            }
        }
        {
            let mut data = self.data.lock().await;
            if let Some(debug_name) = self.debug_name {
                tracyrs::emit_plot(debug_name, data.len() as f64);
            }
            for _ in 0..permits {
                let elem = data.pop_front().expect("invalid WorkQ state");
                buffer.push(elem);
            }
            if let Some(debug_name) = self.debug_name {
                tracyrs::emit_plot(debug_name, data.len() as f64);
            }
        }
        self.free.add_permits(permits);
    }
}
