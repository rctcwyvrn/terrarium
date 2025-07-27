pub mod simple_executor;

use alloc::boxed::Box;
use core::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

pub struct UnitDeferred {
    future: Pin<Box<dyn Future<Output = ()>>>,
}

impl UnitDeferred {
    pub fn new(future: impl Future<Output = ()> + 'static) -> UnitDeferred {
        UnitDeferred {
            future: Box::pin(future),
        }
    }
    fn poll(&mut self, context: &mut Context) -> Poll<()> {
        self.future.as_mut().poll(context)
    }
}
