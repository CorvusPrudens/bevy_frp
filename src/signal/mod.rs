use super::{
    DeferredRegistration,
    derive::MaybeChanged,
    graph::{ReactiveNode, SignalObserver},
};
use crate::{Effect, ReactScheduleSystems};
use bevy_app::prelude::*;
use bevy_ecs::{prelude::*, relationship::Relationship, system::ReadOnlySystemParam};
use bevy_platform::collections::HashSet;
use std::{
    any::TypeId,
    sync::{Arc, RwLock, atomic::AtomicU32},
};

pub mod derived;
pub mod observer;
pub mod rw_signal;

// pub use derived::{MappedSignal, MappedSignalOpt, Signal, SignalOpt, SignalReadGuard};
// pub use rw_signal::{ReadSignal, RwSignal, WriteSignal};

pub(super) struct SignalPlugin;

impl Plugin for SignalPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PostUpdate,
            report_signals.before(super::ReactSystems::EvaluateReactions),
        );
    }
}

fn report_signals(
    signals: Query<Entity, With<SignalSatellite>>,
    mut count: Local<usize>,
    // mut commands: Commands,
) {
    let total = signals.iter().len();
    if total != *count {
        *count = total;
        let s = if total == 1 { "" } else { "s" };
        log::debug!("{total} signal{s}");

        // if total > 0 {
        //     for signal in signals.iter() {
        //         commands.entity(signal).log_components();
        //     }
        // }
    }
}

#[derive(Component, Default)]
struct SignalSatellite;

#[derive(Resource, Default)]
struct RegisteredData(HashSet<TypeId>);

impl super::SignalExt for Commands<'_, '_> {
    fn rw_signal<T>(&mut self, value: T) -> rw_signal::RwSignal<T>
    where
        T: Send + Sync + 'static,
    {
        let inner = Arc::new(rw_signal::RwSignalInner {
            value: RwLock::new(value),
            tick: AtomicU32::new(0),
            subscriber_set: Default::default(),
        });

        let entity = self
            .spawn(rw_signal::RwSignalData {
                value: Arc::clone(&inner),
                change_tick: 0,
            })
            .id();

        rw_signal::RwSignal {
            value: inner,
            entity,
        }
    }

    fn derive<S, O, M>(&mut self, system: S) -> derived::Signal<O>
    where
        S: SystemParamFunction<M, In = (), Out = O> + Send + Sync + 'static,
        S::Param: MaybeChanged + ReadOnlySystemParam,
        M: 'static,
        O: PartialEq + Send + Sync + 'static,
    {
        let entity = self.spawn_empty().id();
        let inner: Arc<derived::SignalInner<O>> = Arc::new(derived::SignalInner {
            value: RwLock::new(None),
            source_set: Default::default(),
            subscriber_set: Default::default(),
            tick: AtomicU32::new(0),
        });
        let changer = Arc::new(|world: &World| S::Param::maybe_changed(world));

        self.queue({
            let inner = inner.clone();
            move |world: &mut World| -> Result {
                if world
                    .get_resource_or_init::<RegisteredData>()
                    .0
                    .insert(TypeId::of::<O>())
                {
                    world
                        .resource_mut::<DeferredRegistration>()
                        .queue(|schedule| {
                            schedule.add_systems((derived::evaluate_signals::<O>
                                .in_set(ReactScheduleSystems::EvaluateSignals),));
                        });
                }

                let system = world.register_system(system);
                let initial_value = SignalObserver::observe(&inner, || world.run_system(system))?;
                *inner.value.write().unwrap() = Some(initial_value);
                inner.mark_dirty();

                world.get_entity_mut(entity)?.insert(derived::SignalData {
                    value: inner,
                    system,
                    changer,
                    // TODO: should we read from inner here?
                    change_tick: 0,
                });

                Ok(())
            }
        });

        derived::Signal {
            entity,
            value: inner,
        }
    }

    fn derive_list<S1, M1, F, I, K, S2, O2, M2, R>(
        &mut self,
        it: S1,
        key: F,
        child: S2,
    ) -> super::ReactiveList<R>
    where
        S1: SystemParamFunction<M1, In = (), Out = Vec<I>> + Send + Sync + 'static,
        S1::Param: MaybeChanged + ReadOnlySystemParam,
        M1: 'static,
        F: Fn(&I) -> K + Send + Sync + 'static,
        I: PartialEq + Clone + Send + Sync + 'static,
        K: Eq + core::hash::Hash + Clone + Send + Sync + 'static,
        S2: IntoSystem<In<I>, O2, M2> + 'static,
        S2::System: ReadOnlySystem,
        O2: Bundle,
        R: Relationship,
    {
        let collection = self.derive(it);
        let key = Box::new(key) as Box<dyn Fn(&I) -> K + Send + Sync>;
        let child = self.register_system(child);

        super::list::reactive_list(self, collection, key, child)
    }

    fn has<C>(&mut self, entity: Entity) -> rw_signal::ReadSignal<bool>
    where
        C: Component,
    {
        let (has, write_has) = self.signal(false);

        self.queue(move |world: &mut World| -> Result {
            // If it's not `Some`, then it definitely doesn't have it yet!
            if let Some(id) = world.component_id::<C>() {
                let current_has = world.get_entity(entity)?.archetype().contains(id);
                // no need to mark it dirty if it's still false
                if current_has {
                    write_has.set(true);
                }
            }

            world
                .get_entity_mut(entity)?
                .observe({
                    let write_has = write_has.clone();
                    move |_: On<Remove, C>| {
                        write_has.set(false);
                    }
                })
                .observe(move |_: On<Add, C>| {
                    write_has.set(true);
                });

            Ok(())
        });

        has
    }

    fn effect<S, M>(&mut self, system: S) -> Effect
    where
        S: IntoSystem<(), (), M> + Send + Sync + 'static,
        M: 'static,
    {
        Effect::new(system, self.reborrow())
    }
}
