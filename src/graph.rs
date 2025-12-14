use std::{
    cell::RefCell,
    sync::{Arc, Weak},
};

pub type AnySource = Weak<dyn Source + Send + Sync>;
pub type AnySubscriber = Weak<dyn Subscriber + Send + Sync>;

pub trait ReactiveNode {
    fn mark_dirty(&self);
}

pub trait Subscriber: ReactiveNode {
    fn add_source(&self, source: AnySource);
    fn clear_sources(&self, subscriber: &AnySubscriber);
}

/// Describes the behavior of any source of reactivity (like a signal, trigger, or memo.)
pub trait Source: ReactiveNode {
    /// Adds a subscriber to this source's list of dependencies.
    fn add_subscriber(&self, subscriber: AnySubscriber);

    /// Removes a subscriber from this source's list of dependencies.
    fn remove_subscriber(&self, subscriber: &AnySubscriber);

    /// Remove all subscribers from this source's list of dependencies.
    fn clear_subscribers(&self);
}

#[derive(Default)]
pub struct SubscriberSet(Vec<AnySubscriber>);

impl SubscriberSet {
    pub fn insert(&mut self, subscriber: AnySubscriber) {
        if !self.0.iter().any(|s| Weak::ptr_eq(s, &subscriber)) {
            self.0.push(subscriber);
        }
    }

    pub fn remove(&mut self, subscriber: &AnySubscriber) {
        self.0.retain(|s| !Weak::ptr_eq(s, subscriber));
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn mark_dirty(&self) {
        for node in self.0.iter().filter_map(|n| n.upgrade()) {
            node.mark_dirty();
        }
    }
}

#[derive(Default)]
pub struct SourceSet(Vec<AnySource>);

impl SourceSet {
    pub fn insert(&mut self, source: AnySource) {
        if !self.0.iter().any(|s| Weak::ptr_eq(s, &source)) {
            self.0.push(source);
        }
    }

    pub fn clear_sources(&mut self, subscriber: &AnySubscriber) {
        for source in self.0.drain(..).filter_map(|s| s.upgrade()) {
            source.remove_subscriber(subscriber);
        }
    }
}

thread_local! {
    static OBSERVER: RefCell<Option<ObserverState>> = const { RefCell::new(None) };
}

#[derive(Debug)]
struct ObserverState {
    subscriber: AnySubscriber,
}

/// The current reactive observer.
///
/// The observer is whatever reactive node is currently listening for signals that need to be
/// tracked. For example, if an effect is running, that effect is the observer, which means it will
/// subscribe to changes in any signals that are read.
pub struct SignalObserver;

impl SignalObserver {
    pub fn observe<S, F, O>(subscriber: &Arc<S>, f: F) -> O
    where
        S: Subscriber + Send + Sync + 'static,
        F: FnOnce() -> O,
    {
        Self::set(Some(
            Arc::downgrade(subscriber) as Weak<dyn Subscriber + Send + Sync>
        ));
        let result = f();
        Self::take();

        result
    }

    /// Returns the current observer, if any.
    pub fn get() -> Option<Arc<dyn Subscriber + Send + Sync>> {
        OBSERVER.with_borrow(|obs| {
            obs.as_ref()
                .and_then(|o| Weak::upgrade(&o.subscriber).clone())
        })
    }

    fn take() -> Option<AnySubscriber> {
        OBSERVER.with_borrow_mut(Option::take).map(|o| o.subscriber)
    }

    fn set(observer: Option<AnySubscriber>) {
        OBSERVER.with_borrow_mut(|o| *o = observer.map(|subscriber| ObserverState { subscriber }));
    }
}
