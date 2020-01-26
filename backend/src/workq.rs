// TODO: replace this once tokio provides a mpmc channel?

// use futures_intrusive::sync::Semaphore;
use std::collections::VecDeque;
use std::sync::Arc;
use tokio::sync::{Mutex, Semaphore};

pub(crate) struct WorkQ<T> {
    data: Mutex<VecDeque<T>>,
    free: Semaphore,
    ready: Semaphore,
    size: usize,
    debug_name: Option<&'static [u8]>,
}

impl<T> WorkQ<T> {
    pub(crate) fn new(size: usize, debug_name: Option<&'static [u8]>) -> Arc<Self> {
        Arc::new(WorkQ {
            data: Mutex::new(VecDeque::with_capacity(size)),
            free: Semaphore::new(size),
            ready: Semaphore::new(0),
            size,
            debug_name,
        })
    }

    pub(crate) async fn push(&self, elem: T) {
        if let Ok(permit) = self.free.try_acquire() {
            permit.forget();
        } else {
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

// futures-intrusive Semaphore:
/*
impl<T> WorkQ<T> {
    pub(crate) fn new(size: usize, debug_name: &'static [u8]) -> Arc<Self> {
        Arc::new(WorkQ {
            data: Mutex::new(VecDeque::with_capacity(size)),
            free: Semaphore::new(false, size),
            ready: Semaphore::new(false, 0),
            debug_name,
        })
    }

    pub(crate) async fn push(&self, elem: T) {
        if let Some(mut permit) = self.free.try_acquire(1) {
            permit.disarm();
        } else {
            tracyrs::message!("blocking WorkQ::push");
            self.free.acquire(1).await.disarm();
            tracyrs::message!("~blocking WorkQ::push");
        }
        {
            let mut data = self.data.lock().await;
            data.push_back(elem);
            tracyrs::emit_plot(self.debug_name, data.len() as f64);
        }
        self.ready.release(1);
    }

    pub(crate) async fn pop(&self) -> T {
        self.ready.acquire(1).await.disarm();
        let res;
        {
            let mut data = self.data.lock().await;
            res = data.pop_front().expect("invalid WorkQ state");
            tracyrs::emit_plot(self.debug_name, data.len() as f64);
        }
        self.free.release(1);
        res
    }

    pub(crate) async fn pop_batch(&self, buffer: &mut Vec<T>) {
        if let Some(mut permit) = self.ready.try_acquire(1) {
            permit.disarm();
        } else {
            tracyrs::message!("blocking WorkQ::pop_batch");
            self.ready.acquire(1).await.disarm();
            tracyrs::message!("~blocking WorkQ::pop_batch");
        }
        let mut permits = 1;
        while let Some(mut permit) = self.ready.try_acquire(1) {
            permits += 1;
            permit.disarm();
        }
        {
            let mut data = self.data.lock().await;
            tracyrs::emit_plot(self.debug_name, data.len() as f64);
            for _ in 0..permits {
                let elem = data.pop_front().expect("invalid WorkQ state");
                buffer.push(elem);
            }
            tracyrs::emit_plot(self.debug_name, data.len() as f64);
        }
        self.free.release(permits);
    }
}
*/
