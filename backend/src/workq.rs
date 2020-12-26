use std::collections::VecDeque;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use tokio::sync::{Mutex, Semaphore};

/// Futures-aware (work-stealing) mpmc channel with tracy instrumentation.
#[derive(Debug)]
pub struct WorkQ<T> {
    data: Mutex<VecDeque<T>>,
    free: Semaphore,
    ready: Semaphore,
    size: usize,
    debug_name: Option<&'static [u8]>,
    blocking_push_count: AtomicU32,
    blocking_pop_count: AtomicU32,
}

impl<T> WorkQ<T> {
    pub fn new(size: usize, debug_name: Option<&'static [u8]>) -> Arc<Self> {
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

    pub async fn push(&self, elem: T) {
        if let Ok(permit) = self.free.try_acquire() {
            permit.forget();
        } else {
            self.blocking_push_count.fetch_add(1, Ordering::Relaxed);
            tracyrs::message!("blocking WorkQ::push");
            self.free.acquire().await.unwrap().forget();
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

    pub async fn push_batch(&self, buffer: &mut Vec<T>) {
        while !buffer.is_empty() {
            let mut permits = 0;
            while permits < buffer.len() {
                if let Ok(permit) = self.free.try_acquire() {
                    permit.forget();
                    permits += 1;
                } else {
                    break;
                }
            }
            if permits == 0 {
                self.blocking_push_count.fetch_add(1, Ordering::Relaxed);
                tracyrs::message!("blocking WorkQ::push_batch");
                self.free.acquire().await.unwrap().forget();
                tracyrs::message!("~blocking WorkQ::push_batch");
                permits += 1;
            }
            debug_assert!(permits > 0 && permits <= buffer.len());
            let mut data = self.data.lock().await;
            for elem in buffer.drain(..permits) {
                // TODO: this is O(n^2) :(
                data.push_back(elem);
            }
            let new_len = data.len();
            drop(data);
            if let Some(debug_name) = self.debug_name {
                tracyrs::emit_plot(debug_name, new_len as f64);
            }
            self.ready.add_permits(permits);
        }
    }

    pub async fn pop(&self) -> T {
        self.ready.acquire().await.unwrap().forget();
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

    pub async fn pop_batch(&self, buffer: &mut Vec<T>) {
        if let Ok(permit) = self.ready.try_acquire() {
            permit.forget();
        } else {
            self.blocking_pop_count.fetch_add(1, Ordering::Relaxed);
            tracyrs::message!("blocking WorkQ::pop_batch");
            self.ready.acquire().await.unwrap().forget();
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

    pub async fn try_drain(&self) {
        let mut permits = 0;
        while let Ok(permit) = self.ready.try_acquire() {
            permits += 1;
            permit.forget();
        }
        {
            let mut data = self.data.lock().await;
            for _ in 0..permits {
                data.pop_front().expect("invalid WorkQ state");
            }
        }
        self.free.add_permits(permits);
    }
}
