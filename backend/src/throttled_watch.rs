use std::{pin::Pin, time::Duration};

use futures::Stream;
use tokio::sync::watch::Receiver;
use tokio_stream::wrappers::WatchStream;
use tokio_stream::StreamExt;

pub struct ThrottledWatch<T: Clone + Send + Sync> {
    stream: Pin<Box<dyn Stream<Item = T> + Send + Sync>>,
}

impl<T: Clone + Send + Sync + 'static + Unpin> ThrottledWatch<T> {
    pub fn new(rx: Receiver<T>) -> Self {
        let rx = WatchStream::new(rx);
        let rate = Duration::from_millis(250);
        let stream = Box::pin(rx.throttle(rate));
        ThrottledWatch { stream }
    }
}

impl<T: Clone + Send + Sync> futures::Stream for ThrottledWatch<T> {
    type Item = T;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        self.stream.as_mut().poll_next(cx)
    }
}
