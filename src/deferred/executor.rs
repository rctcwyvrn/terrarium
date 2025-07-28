use super::{DeferredId, UnitDeferred};
use alloc::{collections::BTreeMap, sync::Arc, task::Wake};
use core::task::{Context, Poll, Waker};
use crossbeam_queue::ArrayQueue;

pub struct Executor {
    deferreds: BTreeMap<DeferredId, UnitDeferred>,
    queue: Arc<ArrayQueue<DeferredId>>,
    waker_cache: BTreeMap<DeferredId, Waker>,
}

static EXECUTOR_QUEUE_CAP: usize = 100;

impl Executor {
    pub fn spawn(&mut self, def: UnitDeferred) {
        let id = def.id;
        if self.deferreds.insert(id, def).is_some() {
            panic!("deferred with same ID already in tasks");
        }
        self.queue.push(id).expect("queue full");
    }

    fn run_ready_deferreds(&mut self) {
        while let Some(id) = self.queue.pop() {
            let def = match self.deferreds.get_mut(&id) {
                Some(task) => task,
                None => continue, // task no longer exists
            };

            // grab the existing waker from the cache for this task,
            // or create it if it doesn't exist, cloning the entire task queue
            let waker = self
                .waker_cache
                .entry(id)
                .or_insert_with(|| DeferredWaker::new(id, self.queue.clone()));
            // create a context and poll with it
            let mut context = Context::from_waker(waker);
            match def.poll(&mut context) {
                Poll::Ready(()) => {
                    // task done -> remove it and its cached waker
                    self.deferreds.remove(&id);
                    self.waker_cache.remove(&id);
                }
                Poll::Pending => {
                    // do nothing, so this id is still in the btree and the waker cache
                    // but it's no longer in the queue
                    //
                    // it's expected that the waker will re-add it to the queue when it gets woken up
                }
            }
        }
    }

    pub fn run(&mut self) -> ! {
        loop {
            // run deferreds
            self.run_ready_deferreds();

            // sleep until the next interrupt if there are no deferreds
            // we have to disable/enable interrupts to prevent an interrupt from happening
            // between the queue.is_empty() call and the hlt
            x86_64::instructions::interrupts::disable();
            if self.queue.is_empty() {
                x86_64::instructions::interrupts::enable_and_hlt();
            } else {
                x86_64::instructions::interrupts::enable();
            }
        }
    }

    pub fn new() -> Self {
        Executor {
            deferreds: BTreeMap::new(),
            queue: Arc::new(ArrayQueue::new(EXECUTOR_QUEUE_CAP)),
            waker_cache: BTreeMap::new(),
        }
    }
}

struct DeferredWaker {
    id: DeferredId,
    queue: Arc<ArrayQueue<DeferredId>>,
}

impl DeferredWaker {
    fn wake_task(&self) {
        // to wake is to push it on to the deferred queue
        self.queue
            .push(self.id)
            .expect("executor deferred queue full");
    }

    fn new(id: DeferredId, queue: Arc<ArrayQueue<DeferredId>>) -> Waker {
        Waker::from(Arc::new(DeferredWaker { id, queue }))
    }
}

impl Wake for DeferredWaker {
    fn wake(self: Arc<Self>) {
        self.wake_task();
    }

    fn wake_by_ref(self: &Arc<Self>) {
        self.wake_task();
    }
}
