use bevy_app::prelude::*;
use bevy_ecs::{
    change_detection::MaybeLocation, prelude::*, relationship::Relationship,
    schedule::ScheduleLabel, system::ReadOnlySystemParam,
};

#[cfg(debug_assertions)]
use bevy_platform::collections::HashSet;

mod any;
mod derive;
mod effect;
mod graph;
mod list;
mod signal;
mod target;

pub use any::{AnyComponent, IntoAnyComponent};
pub use effect::Effect;
pub use list::ReactiveList;
pub use signal::{
    derived::{
        MappedSignal, MappedSignalOpt, Signal, SignalOpt, SignalReadGuard, SignalWriteGuard,
    },
    observer::SignalObserver,
    rw_signal::{ReadSignal, RwSignal, WriteSignal},
};
pub use target::{TQuery, Target, TargetQueryError, Targets};

pub struct ReactPlugin;

impl Plugin for ReactPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Reactions>()
            .init_resource::<DeferredRegistration>()
            .init_schedule(ReactSchedule)
            .configure_sets(PostUpdate, ReactSystems::EvaluateReactions)
            .configure_sets(
                ReactSchedule,
                (
                    ReactScheduleSystems::EvaluateSignals,
                    ReactScheduleSystems::EvaluateEffects
                        .after(ReactScheduleSystems::EvaluateSignals),
                    ReactScheduleSystems::PropagateChanges
                        .after(ReactScheduleSystems::EvaluateEffects),
                ),
            )
            .add_plugins((
                signal::SignalPlugin,
                target::TargetPlugin,
                effect::EffectPlugin,
                list::ReactiveListPlugin,
            ))
            .add_systems(
                PostUpdate,
                evaluate_reactions.in_set(ReactSystems::EvaluateReactions),
            );
    }
}

#[derive(Resource, Default)]
struct DeferredRegistration(Vec<Box<dyn FnOnce(&mut Schedule) + Send + Sync>>);

impl DeferredRegistration {
    pub fn queue(&mut self, func: impl FnOnce(&mut Schedule) + Send + Sync + 'static) {
        self.0.push(Box::new(func));
    }
}

#[derive(Resource)]
pub struct Reactions {
    reaction_limit: usize,
    count: usize,
    #[cfg(debug_assertions)]
    locations: HashSet<MaybeLocation>,
}

impl Default for Reactions {
    fn default() -> Self {
        Self::new(16)
    }
}

impl Reactions {
    pub fn new(reaction_limit: usize) -> Self {
        Self {
            reaction_limit,
            count: 0,
            #[cfg(debug_assertions)]
            locations: Default::default(),
        }
    }

    #[cfg_attr(debug_assertions, track_caller)]
    pub fn increment(&mut self) {
        self.count += 1;
        #[cfg(debug_assertions)]
        self.locations.insert(MaybeLocation::caller());
    }

    fn clear(&mut self) {
        self.count = 0;
        #[cfg(debug_assertions)]
        self.locations.clear();
    }
}

fn evaluate_reactions(world: &mut World) {
    world.schedule_scope(ReactSchedule, |world, schedule| {
        let mut total = 0;
        let reaction_limit = world.resource::<Reactions>().reaction_limit;

        #[cfg(debug_assertions)]
        #[derive(Default)]
        struct ReactionReport {
            locations: HashSet<MaybeLocation>,
            counts: Vec<usize>,
        }

        #[cfg(debug_assertions)]
        let mut report = ReactionReport::default();

        for _ in 0..reaction_limit {
            world.resource_scope::<DeferredRegistration, _>(|_world, mut registration| {
                for reg in registration.0.drain(..) {
                    reg(schedule);
                }
            });

            world.resource_mut::<Reactions>().clear();

            schedule.run(world);

            total += 1;
            let reactions = world.resource::<Reactions>();
            let new_reactions = reactions.count;

            #[cfg(debug_assertions)]
            {
                report.locations.extend(reactions.locations.iter().cloned());
                report.counts.push(new_reactions);
            }

            if new_reactions == 0 {
                break;
            }
        }

        #[cfg(debug_assertions)]
        {
            if !matches!(report.counts.as_slice(), &[0]) {
                let mut locations = report
                    .locations
                    .iter()
                    .map(|l| format!("{l}"))
                    .collect::<Vec<_>>();
                locations.sort_unstable();

                log::debug!("Reactive evaluation report:");
                log::debug!(
                    "Reaction counts ({}): {:?}",
                    report.counts.len(),
                    report.counts
                );
                log::debug!("Locations: {:#?}", locations);
            }
        }

        if total == reaction_limit {
            log::warn!("Reached reactive evaluation limit");
        }
    });
}

#[derive(ScheduleLabel, PartialEq, Eq, Clone, Debug, Hash)]
pub struct ReactSchedule;

#[derive(SystemSet, PartialEq, Eq, Clone, Debug, Hash)]
pub enum ReactSystems {
    EvaluateReactions,
}

#[derive(SystemSet, PartialEq, Eq, Clone, Debug, Hash)]
pub enum ReactScheduleSystems {
    EvaluateSignals,
    EvaluateEffects,
    PropagateChanges,
}

pub trait SignalExt {
    #[inline(always)]
    fn signal<T>(&mut self, value: T) -> (ReadSignal<T>, WriteSignal<T>)
    where
        T: Send + Sync + 'static,
    {
        self.rw_signal(value).split()
    }

    fn rw_signal<T>(&mut self, value: T) -> RwSignal<T>
    where
        T: Send + Sync + 'static;

    fn derive<S, O, M>(&mut self, system: S) -> Signal<O>
    where
        S: SystemParamFunction<M, In = (), Out = O> + Send + Sync + 'static,
        S::Param: derive::MaybeChanged + ReadOnlySystemParam,
        M: 'static,
        O: PartialEq + Send + Sync + 'static;

    fn derive_list<S1, M1, F, I, K, S2, O2, M2, R>(
        &mut self,
        it: S1,
        key: F,
        child: S2,
    ) -> ReactiveList<R>
    where
        S1: SystemParamFunction<M1, In = (), Out = Vec<I>> + Send + Sync + 'static,
        S1::Param: derive::MaybeChanged + ReadOnlySystemParam,
        M1: 'static,
        I: PartialEq + Clone + Send + Sync + 'static,
        F: Fn(&I) -> K + Send + Sync + 'static,
        K: Eq + core::hash::Hash + Clone + Send + Sync + 'static,
        S2: IntoSystem<In<I>, O2, M2> + 'static,
        S2::System: ReadOnlySystem,
        O2: Bundle,
        R: Relationship;

    fn has<C>(&mut self, entity: Entity) -> ReadSignal<bool>
    where
        C: Component;

    #[must_use]
    fn effect<S, M>(&mut self, system: S) -> Effect
    where
        S: IntoSystem<(), (), M> + Send + Sync + 'static,
        // S: SystemParamFunction<M, In = (), Out = ()> + Send + Sync + 'static,
        // S::Param: derive::IsChanged,
        M: 'static;
}

#[macro_export]
macro_rules! spawn_element {
    ($func:path, $commands:expr, $($args:expr),*$(,)?) => {
        {
            let entity = $func($commands.reborrow(), $($args),*);
            $commands.spawn(entity)
        }
    };
}
