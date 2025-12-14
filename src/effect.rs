use super::{
    ReactSchedule, ReactScheduleSystems, Reactions,
    derive::IsChanged,
    graph::{AnySubscriber, ReactiveNode, SignalObserver, SourceSet, Subscriber},
};
use bevy_app::prelude::*;
use bevy_ecs::{lifecycle::HookContext, prelude::*, system::SystemId, world::DeferredWorld};
use std::sync::{
    Arc, RwLock, Weak,
    atomic::{AtomicU32, Ordering},
};

pub struct EffectPlugin;

impl Plugin for EffectPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            ReactSchedule,
            evaluate_effects.in_set(ReactScheduleSystems::EvaluateEffects),
        );
    }
}

struct EffectInner {
    tick: AtomicU32,
    source_set: RwLock<SourceSet>,
}

impl ReactiveNode for EffectInner {
    fn mark_dirty(&self) {
        self.tick.fetch_add(1, Ordering::Relaxed);
    }
}

impl Subscriber for EffectInner {
    fn add_source(&self, source: super::graph::AnySource) {
        self.source_set.write().unwrap().insert(source);
    }

    fn clear_sources(&self, subscriber: &AnySubscriber) {
        self.source_set.write().unwrap().clear_sources(subscriber);
    }
}

#[derive(Component, Clone)]
pub struct Effect(Arc<EffectInner>);

#[derive(Component, Clone)]
#[component(on_replace = Self::on_replace_hook)]
struct EffectData {
    effect: Effect,
    changer: Arc<dyn Fn(&World) -> bool + Send + Sync>,
    system: SystemId,
    change_tick: u32,
}

impl EffectData {
    fn on_replace_hook(mut world: DeferredWorld, context: HookContext) {
        let Some(data) = world.get::<Self>(context.entity) else {
            return;
        };

        let system = data.system;
        world.commands().unregister_system(system);
    }
}

impl Effect {
    pub(super) fn new<S, M>(system: S, mut commands: Commands) -> Self
    where
        // S: SystemParamFunction<M, In = (), Out = ()> + Send + Sync + 'static,
        S: IntoSystem<(), (), M> + Send + Sync + 'static,
        // S::Param: IsChanged,
        M: 'static,
    {
        // let changer = Arc::new(|world: &World| S::Param::is_changed(world));
        let changer = Arc::new(|world: &World| false);
        let effect = Effect(Arc::new(EffectInner {
            source_set: Default::default(),
            tick: AtomicU32::new(0),
        }));

        commands.queue({
            let effect = effect.clone();
            move |world: &mut World| -> Result {
                let system = world.register_system(system);

                let result = SignalObserver::observe(&effect.0, || world.run_system(system));
                if let Err(e) = result {
                    log::error!("Initial effect failed: {e:?}");
                }

                world.spawn(EffectData {
                    effect,
                    changer,
                    system,
                    change_tick: 0,
                });

                Ok(())
            }
        });

        effect
    }
}

fn evaluate_effects(world: &mut World) -> Result {
    let mut systems = world.query::<(Entity, &EffectData)>();
    let systems: Vec<_> = systems.iter(world).map(|(e, s)| (e, s.clone())).collect();

    for (
        entity,
        EffectData {
            effect,
            changer,
            system,
            change_tick,
        },
    ) in systems
    {
        let tick = effect.0.tick.load(Ordering::Relaxed);
        let dirty = tick != change_tick;
        if !(dirty || changer(world)) {
            let Ok(entity) = world.get_entity_mut(entity) else {
                continue;
            };

            let Some(current_data) = entity.get::<EffectData>() else {
                continue;
            };

            if Arc::strong_count(&current_data.effect.0) == 2 {
                entity.despawn();
            }

            continue;
        }

        let subscriber = Arc::downgrade(&effect.0) as Weak<dyn Subscriber + Send + Sync>;
        effect.0.clear_sources(&subscriber);
        let result = SignalObserver::observe(&effect.0, || world.run_system(system));

        let Ok(mut entity_mut) = world.get_entity_mut(entity) else {
            continue;
        };

        let Some(mut current_data) = entity_mut.get_mut::<EffectData>() else {
            continue;
        };

        if Arc::strong_count(&current_data.effect.0) <= 2 {
            entity_mut.despawn();
        } else {
            current_data.change_tick = tick;
            world.resource_mut::<Reactions>().increment();
        }

        if let Err(e) = result {
            log::error!("Effect error: {e}");
        }
    }

    Ok(())
}
