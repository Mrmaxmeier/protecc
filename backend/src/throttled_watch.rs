use std::time::Duration;

use futures::{Future, FutureExt, Stream};
use stream_throttle::{ThrottlePool, ThrottleRate};
use tokio::sync::watch::Receiver;

pub struct ThrottledWatch<T: Clone + Send + Sync> {
    recv: Receiver<T>,
    pool: ThrottlePool,
    // stream: async_stream::AsyncStream<T, ()>,
    // stream: Box<dyn Stream<Item = T> + Send + Sync>,
}

impl<T: Clone + Send + Sync + 'static + Unpin> ThrottledWatch<T> {
    pub fn new(mut recv: Receiver<T>) -> Self {
        let rate = ThrottleRate::new(5, Duration::new(1, 0));
        let pool = ThrottlePool::new(rate);

        /*
        let stream = Box::new(async_stream::stream! {
            while let Ok(()) = recv.changed().await {
                yield recv.borrow().clone();
            }
        });
        */
        ThrottledWatch { pool, recv }
    }
}

impl<T: Clone + Send + Sync> futures::Stream for ThrottledWatch<T> {
    type Item = T;

    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        // self.recv.changed();
        // FIXME TODO XXX: this doesn't wait for recv.changed() !!
        match self.pool.queue().poll_unpin(cx) {
            std::task::Poll::Ready(_) => std::task::Poll::Ready(Some(self.recv.borrow().clone())),
            std::task::Poll::Pending => std::task::Poll::Pending,
        }
        // todo!()
    }
}
