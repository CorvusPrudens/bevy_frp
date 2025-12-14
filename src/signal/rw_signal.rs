use crate::{
    DeferredRegistration,
    graph::{AnySubscriber, ReactiveNode, SignalObserver, Source, Subscriber, SubscriberSet},
};
use crate::{ReactSchedule, ReactScheduleSystems};
use bevy_ecs::{lifecycle::HookContext, prelude::*, world::DeferredWorld};
use bevy_platform::collections::HashSet;
use std::{
    any::TypeId,
    marker::PhantomData,
    sync::{
        Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak,
        atomic::{AtomicU32, Ordering},
    },
};

pub struct RwSignal<T> {
    pub(super) value: Arc<RwSignalInner<T>>,
    pub(super) entity: Entity,
}

impl<T: Send + Sync + 'static> RwSignal<T> {
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        self.value.read()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, T> {
        self.value.write()
    }

    pub fn set(&self, value: T) {
        *self.value.write() = value;
    }

    pub fn split(self) -> (ReadSignal<T>, WriteSignal<T>) {
        (ReadSignal(self.clone()), WriteSignal(self))
    }
}

trait RwSignalRead<T> {
    fn read(&self) -> RwLockReadGuard<'_, T>;
}

impl<T> RwSignalRead<T> for Arc<RwSignalInner<T>>
where
    T: Send + Sync + 'static,
{
    fn read(&self) -> RwLockReadGuard<'_, T> {
        if let Some(observer) = SignalObserver::get() {
            observer.add_source(Arc::downgrade(self) as Weak<dyn Source + Send + Sync>);
            self.add_subscriber(Arc::downgrade(&observer) as Weak<dyn Subscriber + Send + Sync>);
        }

        self.value.read().unwrap()
    }
}

trait RwSignalWrite<T> {
    fn write(&self) -> RwLockWriteGuard<'_, T>;
}

impl<T> RwSignalWrite<T> for Arc<RwSignalInner<T>>
where
    T: Send + Sync + 'static,
{
    fn write(&self) -> RwLockWriteGuard<'_, T> {
        self.mark_dirty();
        self.value.write().unwrap()
    }
}

impl<T: Clone + Send + Sync + 'static> RwSignal<T> {
    pub fn get(&self) -> T {
        self.value.read().clone()
    }
}

impl<T> Clone for RwSignal<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
            entity: self.entity,
        }
    }
}

pub struct ReadSignal<T>(RwSignal<T>);

impl<T: Send + Sync + 'static> ReadSignal<T> {
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        self.0.read()
    }
}

impl<T: Clone + Send + Sync + 'static> ReadSignal<T> {
    pub fn get(&self) -> T {
        self.0.get()
    }
}

impl<T> Clone for ReadSignal<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub struct WriteSignal<T>(RwSignal<T>);

impl<T: Send + Sync + 'static> WriteSignal<T> {
    pub fn write(&self) -> RwLockWriteGuard<'_, T> {
        self.0.write()
    }

    pub fn set(&self, value: T) {
        *self.0.write() = value;
    }
}

impl<T> Clone for WriteSignal<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[derive(Resource, Default)]
struct RegisteredRwSignals(HashSet<TypeId>);

// A manual derive of `Component`, applying only when
// the signal contains a value that also implements `Component`.
impl<T> bevy_ecs::component::Component for RwSignal<T>
where
    Self: Send + Sync + 'static,
    T: Bundle + Clone,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Immutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Bundle + Clone>(mut world: DeferredWorld, _context: HookContext) {
            if world
                .get_resource::<RegisteredRwSignals>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<T>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredRwSignals>()
                        .0
                        .insert(TypeId::of::<T>())
                    {
                        world.schedule_scope(ReactSchedule, |_, schedule| {
                            schedule.add_systems(
                                propagate_rw_changes::<T>
                                    .in_set(ReactScheduleSystems::PropagateChanges),
                            );
                        });
                    }
                });
            }
        }

        Some(on_add::<T>)
    }

    fn on_insert() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_insert)
    }

    fn on_replace() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_replace)
    }

    fn clone_behavior() -> bevy_ecs::component::ComponentCloneBehavior {
        bevy_ecs::component::ComponentCloneBehavior::Default
    }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.entity.map_entities(mapper);
    }
}

impl<T> bevy_ecs::relationship::Relationship for RwSignal<T>
where
    T: Bundle + Clone,
{
    type RelationshipTarget = RwSignalComponents<T>;

    #[inline(always)]
    fn get(&self) -> bevy_ecs::entity::Entity {
        self.entity
    }

    #[inline(always)]
    fn set_risky(&mut self, entity: Entity) {
        self.entity = entity;
    }

    #[inline]
    fn from(_entity: bevy_ecs::entity::Entity) -> Self {
        panic!("cannot construct `RwSignal` from entity")
    }
}

// A manual derive of `Component`, applying only when
// the signal contains a value that also implements `Component`.
impl<T> bevy_ecs::component::Component for ReadSignal<T>
where
    Self: Send + Sync + 'static,
    T: Bundle + Clone,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Immutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Bundle + Clone>(mut world: DeferredWorld, _context: HookContext) {
            if world
                .get_resource::<RegisteredRwSignals>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<T>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredRwSignals>()
                        .0
                        .insert(TypeId::of::<T>())
                    {
                        world
                            .resource_mut::<DeferredRegistration>()
                            .queue(|schedule| {
                                schedule.add_systems(
                                    propagate_rw_changes::<T>
                                        .in_set(ReactScheduleSystems::PropagateChanges),
                                );
                            });
                    }
                });
            }
        }

        Some(on_add::<T>)
    }

    fn on_insert() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_insert)
    }

    fn on_replace() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_replace)
    }

    fn clone_behavior() -> bevy_ecs::component::ComponentCloneBehavior {
        bevy_ecs::component::ComponentCloneBehavior::Default
    }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.0.entity.map_entities(mapper);
    }
}

impl<T> bevy_ecs::relationship::Relationship for ReadSignal<T>
where
    T: Bundle + Clone,
{
    type RelationshipTarget = ReadSignalComponents<T>;
    #[inline(always)]
    fn get(&self) -> bevy_ecs::entity::Entity {
        self.0.entity
    }

    #[inline(always)]
    fn set_risky(&mut self, entity: Entity) {
        self.0.entity = entity;
    }

    #[inline]
    fn from(_entity: bevy_ecs::entity::Entity) -> Self {
        panic!("cannot construct `RwSignal` from entity")
    }
}

pub(super) struct RwSignalInner<T> {
    pub(super) value: RwLock<T>,
    pub(super) tick: AtomicU32,
    pub(super) subscriber_set: RwLock<SubscriberSet>,
}

// impl<T> RwSignalInner<T> {
//     pub fn read(&self) -> RwLockReadGuard<'_, T> {
//         self.value.read().unwrap()
//     }
//
//     pub fn write(&self) -> RwLockWriteGuard<'_, T> {
//         self.mark_dirty();
//         self.value.write().unwrap()
//     }
// }

impl<T> ReactiveNode for RwSignalInner<T> {
    fn mark_dirty(&self) {
        self.tick.fetch_add(1, Ordering::Relaxed);
        self.subscriber_set.write().unwrap().mark_dirty();
    }
}

impl<T> Source for RwSignalInner<T> {
    fn add_subscriber(&self, subscriber: AnySubscriber) {
        self.subscriber_set.write().unwrap().insert(subscriber);
    }

    fn remove_subscriber(&self, subscriber: &AnySubscriber) {
        self.subscriber_set.write().unwrap().remove(subscriber);
    }

    fn clear_subscribers(&self) {
        self.subscriber_set.write().unwrap().clear();
    }
}

#[derive(Component)]
#[relationship_target(relationship = RwSignal<T>)]
pub struct RwSignalComponents<T: Bundle + Clone> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> T>,
}

#[derive(Component)]
#[relationship_target(relationship = ReadSignal<T>)]
pub struct ReadSignalComponents<T: Bundle + Clone> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> T>,
}

fn propagate_rw_changes<T: Bundle + Clone>(
    mut signals: Query<(
        Entity,
        Mut<RwSignalData<T>>,
        Option<Ref<RwSignalComponents<T>>>,
        Option<Ref<ReadSignalComponents<T>>>,
    )>,
    mut commands: Commands,
) {
    for (entity, mut data, components, read_components) in &mut signals {
        match (components, read_components) {
            (a, b) if a.is_some() || b.is_some() => {
                let atomic_tick = data.value.tick.load(Ordering::Relaxed);

                let new_subscribers = a.as_ref().is_some_and(|a| a.is_changed())
                    || b.as_ref().is_some_and(|b| b.is_changed());

                if data.is_added() || atomic_tick != data.change_tick || new_subscribers {
                    data.change_tick = atomic_tick;
                    for target in a
                        .iter()
                        .flat_map(|a| a.iter())
                        .chain(b.iter().flat_map(|b| b.iter()))
                    {
                        commands.entity(target).insert(data.value.read().clone());
                    }
                }
            }
            _ => {
                if Arc::strong_count(&data.value) == 1 {
                    if let Ok(mut entity) = commands.get_entity(entity) {
                        entity.try_despawn();
                    }
                }
            }
        }
    }
}

fn manage_rw_signals<T: Send + Sync + 'static>(
    signals: Query<(Entity, &RwSignalData<T>)>,
    mut commands: Commands,
) {
    for (entity, data) in &signals {
        if Arc::strong_count(&data.value) == 1 {
            if let Ok(mut entity) = commands.get_entity(entity) {
                entity.try_despawn();
            }
        }
    }
}

#[derive(Component)]
#[require(super::SignalSatellite)]
#[component(on_add = Self::on_add_hook, on_replace = Self::on_replace_hook)]
pub(super) struct RwSignalData<T: Send + Sync> {
    pub(super) value: Arc<RwSignalInner<T>>,
    pub(super) change_tick: u32,
}

#[derive(Resource, Default)]
struct RegisteredRwManagers(HashSet<TypeId>);

impl<T: Send + Sync + 'static> RwSignalData<T> {
    fn on_add_hook(mut world: DeferredWorld, _context: HookContext) {
        if world
            .get_resource::<RegisteredRwManagers>()
            .is_none_or(|r| !r.0.contains(&TypeId::of::<T>()))
        {
            world.commands().queue(|world: &mut World| {
                if world
                    .get_resource_or_init::<RegisteredRwManagers>()
                    .0
                    .insert(TypeId::of::<T>())
                {
                    world
                        .resource_mut::<DeferredRegistration>()
                        .queue(|schedule| {
                            schedule.add_systems(
                                manage_rw_signals::<T>
                                    .in_set(ReactScheduleSystems::PropagateChanges),
                            );
                        });
                }
            });
        }
    }

    fn on_replace_hook(world: DeferredWorld, context: HookContext) {
        let Some(data) = world.get::<Self>(context.entity) else {
            return;
        };

        data.value.clear_subscribers();
    }
}
