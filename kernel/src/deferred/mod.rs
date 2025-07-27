pub mod executor;

use alloc::boxed::Box;
use core::sync::atomic::{AtomicU64, Ordering};
use core::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct DeferredId(u64);

impl DeferredId {
    fn new() -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(0);
        DeferredId(NEXT_ID.fetch_add(1, Ordering::Relaxed))
    }
}

pub struct UnitDeferred {
    id: DeferredId,
    future: Pin<Box<dyn Future<Output = ()>>>,
}

impl UnitDeferred {
    pub fn new(future: impl Future<Output = ()> + 'static) -> UnitDeferred {
        UnitDeferred {
            id: DeferredId::new(),
            future: Box::pin(future),
        }
    }
    fn poll(&mut self, context: &mut Context) -> Poll<()> {
        self.future.as_mut().poll(context)
    }
}
