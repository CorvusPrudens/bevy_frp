use crate::ReactScheduleSystems;
use crate::{
    DeferredRegistration, Reactions,
    graph::{
        AnySubscriber, ReactiveNode, SignalObserver, Source, SourceSet, Subscriber, SubscriberSet,
    },
};
use bevy_ecs::{lifecycle::HookContext, prelude::*, system::SystemId, world::DeferredWorld};
use bevy_platform::collections::HashSet;
use std::{
    any::TypeId,
    marker::PhantomData,
    sync::{
        Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak,
        atomic::{AtomicU32, Ordering},
    },
};

pub struct Signal<T> {
    pub(super) value: Arc<SignalInner<T>>,
    pub(super) entity: Entity,
}

impl<T: Send + Sync + 'static> Signal<T> {
    /// Read the current value.
    ///
    /// # Panics
    ///
    /// Panics if read before the initial value is set.
    /// This will happen if the derived signal is read before
    /// its commands queue is applied.
    pub fn read(&self) -> SignalReadGuard<'_, T> {
        self.value.read()
    }
}

impl<T: Clone + Send + Sync + 'static> Signal<T> {
    /// Clone the current value out of the signal.
    ///
    /// # Panics
    ///
    /// Panics if read before the initial value is set.
    /// This will happen if the derived signal is read before
    /// its commands queue is applied.
    pub fn get(&self) -> T {
        self.value.read().clone()
    }
}

impl<T: Clone + Send + Sync + 'static> Signal<Option<T>> {
    pub fn optional(self) -> SignalOpt<T> {
        SignalOpt {
            entity: self.entity,
            value: self.value,
        }
    }
}

#[derive(Resource, Default)]
struct RegisteredSignals(HashSet<TypeId>);

// A manual derive of `Component`, applying only when
// the signal contains a value that also implements `Component`.
impl<T> bevy_ecs::component::Component for Signal<T>
where
    Self: Send + Sync + 'static,
    T: Bundle + Clone,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Immutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Bundle + Clone>(mut world: DeferredWorld, _context: HookContext) {
            if world
                .get_resource::<RegisteredSignals>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<T>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredSignals>()
                        .0
                        .insert(TypeId::of::<T>())
                    {
                        world
                            .resource_mut::<DeferredRegistration>()
                            .queue(|schedule| {
                                schedule.add_systems(
                                    apply_changes::<T>
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

    // fn clone_behavior() -> bevy_ecs::component::ComponentCloneBehavior {
    //     use bevy_ecs::component::{DefaultCloneBehaviorBase, DefaultCloneBehaviorViaClone};
    //     (&&&bevy_ecs::component::DefaultCloneBehaviorSpecialization::<Self>::default())
    //         .default_clone_behavior()
    // }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.entity.map_entities(mapper);
    }
}

impl<T> bevy_ecs::relationship::Relationship for Signal<T>
where
    T: Bundle + Clone,
{
    type RelationshipTarget = SignalComponents<T>;
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
        panic!("cannot construct `Signal` from entity")
    }
}

impl<T> Signal<T> {
    pub fn entity(&self) -> Entity {
        self.entity
    }
}

impl<T> Clone for Signal<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
            entity: self.entity,
        }
    }
}

pub(super) struct SignalInner<T> {
    pub(super) value: RwLock<Option<T>>,
    pub(super) tick: AtomicU32,
    pub(super) subscriber_set: RwLock<SubscriberSet>,
    pub(super) source_set: RwLock<SourceSet>,
}

impl<T> ReactiveNode for SignalInner<T> {
    fn mark_dirty(&self) {
        self.tick.fetch_add(1, Ordering::Relaxed);
        self.subscriber_set.write().unwrap().mark_dirty();
    }
}

impl<T> Source for SignalInner<T> {
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

impl<T> Subscriber for SignalInner<T> {
    fn add_source(&self, source: crate::graph::AnySource) {
        self.source_set.write().unwrap().insert(source);
    }

    fn clear_sources(&self, subscriber: &AnySubscriber) {
        self.source_set.write().unwrap().clear_sources(subscriber);
    }
}

pub struct SignalReadGuard<'a, T>(RwLockReadGuard<'a, Option<T>>);

impl<'a, T> SignalReadGuard<'a, T> {
    fn new(guard: RwLockReadGuard<'a, Option<T>>) -> Self {
        if guard.is_none() {
            panic!("Attempted to create a signal read guard for a signal without a value");
        }

        Self(guard)
    }
}

impl<T> core::ops::Deref for SignalReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // NOTE: this cannot panic since we check that the inner
        // value `is_some` at construction.
        self.0.as_ref().unwrap()
    }
}

pub struct SignalWriteGuard<'a, T>(RwLockWriteGuard<'a, Option<T>>);

impl<'a, T> SignalWriteGuard<'a, T> {
    fn new(guard: RwLockWriteGuard<'a, Option<T>>) -> Self {
        if guard.is_none() {
            panic!("Attempted to create a signal read guard for a signal without a value");
        }

        Self(guard)
    }
}

impl<T> core::ops::Deref for SignalWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // NOTE: this cannot panic since we check that the inner
        // value `is_some` at construction.
        self.0.as_ref().unwrap()
    }
}

impl<T> core::ops::DerefMut for SignalWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // NOTE: this cannot panic since we check that the inner
        // value `is_some` at construction.
        self.0.as_mut().unwrap()
    }
}

trait SignalRead<T> {
    fn read(&self) -> SignalReadGuard<'_, T>;
}

impl<T> SignalRead<T> for Arc<SignalInner<T>>
where
    T: Send + Sync + 'static,
{
    fn read(&self) -> SignalReadGuard<'_, T> {
        if let Some(observer) = SignalObserver::get() {
            observer.add_source(Arc::downgrade(self) as Weak<dyn Source + Send + Sync>);
            self.add_subscriber(Arc::downgrade(&observer) as Weak<dyn Subscriber + Send + Sync>);
        }

        SignalReadGuard::new(self.value.read().unwrap())
    }
}

trait SignalWrite<T> {
    fn write(&self) -> SignalWriteGuard<'_, T>;
}

impl<T> SignalWrite<T> for Arc<SignalInner<T>>
where
    T: Send + Sync + 'static,
{
    fn write(&self) -> SignalWriteGuard<'_, T> {
        self.mark_dirty();
        SignalWriteGuard::new(self.value.write().unwrap())
    }
}

#[derive(Component)]
#[relationship_target(relationship = Signal<T>)]
pub struct SignalComponents<T: Bundle + Clone> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> T>,
}

fn apply_changes<T: Bundle + Clone>(
    signals: Query<
        (&SignalData<T>, &SignalComponents<T>),
        Or<(Changed<SignalData<T>>, Changed<SignalComponents<T>>)>,
    >,
    mut commands: Commands,
) {
    for (data, components) in &signals {
        for target in components.iter() {
            commands.entity(target).insert(data.value.read().clone());
        }
    }
}

#[derive(Component)]
#[require(super::SignalSatellite)]
#[component(on_replace = Self::on_replace_hook)]
pub(super) struct SignalData<T: Send + Sync> {
    pub(super) value: Arc<SignalInner<T>>,
    pub(super) system: SystemId<(), T>,
    pub(super) changer: Arc<dyn Fn(&World) -> bool + Send + Sync>,
    pub(super) change_tick: u32,
}

impl<T: Send + Sync> Clone for SignalData<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
            system: self.system,
            changer: Arc::clone(&self.changer),
            change_tick: self.change_tick,
        }
    }
}

impl<T: Send + Sync + 'static> SignalData<T> {
    fn on_replace_hook(mut world: DeferredWorld, context: HookContext) {
        let Some(data) = world.get::<Self>(context.entity) else {
            return;
        };

        data.value.clear_subscribers();
        let subscriber = Arc::downgrade(&data.value) as Weak<dyn Subscriber + Send + Sync>;
        data.value.clear_sources(&subscriber);

        let system = data.system;
        world.commands().unregister_system(system);
    }
}

pub(super) fn evaluate_signals<T: PartialEq + Send + Sync + 'static>(world: &mut World) -> Result {
    let mut systems = world.query::<(Entity, &SignalData<T>)>();
    let systems: Vec<_> = systems.iter(world).map(|(e, s)| (e, s.clone())).collect();

    for (
        entity,
        SignalData {
            value,
            system,
            changer,
            change_tick,
        },
    ) in systems
    {
        let tick = value.tick.load(Ordering::Relaxed);
        let dirty = tick != change_tick;
        if !(dirty || changer(world)) {
            let Ok(entity) = world.get_entity_mut(entity) else {
                continue;
            };
            let Some(current_data) = entity.get::<SignalData<T>>() else {
                continue;
            };
            if Arc::strong_count(&current_data.value) == 2 {
                entity.despawn();
            }

            continue;
        }

        let subscriber = Arc::downgrade(&value) as Weak<dyn Subscriber + Send + Sync>;
        value.clear_sources(&subscriber);
        let result = SignalObserver::observe(&value, || world.run_system(system))?;

        let Ok(mut entity_mut) = world.get_entity_mut(entity) else {
            continue;
        };
        let Some(mut current_data) = entity_mut.get_mut::<SignalData<T>>() else {
            continue;
        };

        if Arc::strong_count(&current_data.value) == 2 {
            entity_mut.despawn();
        } else if current_data.value.read().ne(&result) {
            current_data.change_tick = tick;
            *value.write() = result;
            current_data.set_changed();
            world.resource_mut::<Reactions>().increment();
        } else {
            current_data.bypass_change_detection().change_tick = tick;
        }
    }

    Ok(())
}

pub struct MappedSignal<T: 'static, O> {
    signal: Signal<T>,
    system: Option<Box<dyn FnOnce(&mut World) -> SystemId<In<T>, O> + Send + Sync>>,
    handle: Option<SystemId<In<T>, O>>,
}

#[derive(Resource, Default)]
struct RegisteredMappedSignals(HashSet<TypeId>);

impl<T, O> bevy_ecs::component::Component for MappedSignal<T, O>
where
    Self: Send + Sync + 'static,
    T: Clone + Send + Sync + 'static,
    O: Bundle,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Mutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Clone + Send + Sync + 'static, O: Bundle>(
            mut world: DeferredWorld,
            _context: HookContext,
        ) {
            if world
                .get_resource::<RegisteredMappedSignals>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<(T, O)>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredMappedSignals>()
                        .0
                        .insert(TypeId::of::<(T, O)>())
                    {
                        world
                            .resource_mut::<DeferredRegistration>()
                            .queue(|schedule| {
                                schedule.add_systems(
                                    apply_mapped_changes::<T, O>
                                        .in_set(ReactScheduleSystems::PropagateChanges),
                                );
                            });
                    }
                });
            }
        }

        Some(on_add::<T, O>)
    }

    fn on_insert() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_insert<T: Clone + Send + Sync + 'static, O: Bundle>(
            mut world: DeferredWorld,
            context: HookContext,
        ) {
            world.commands().queue(move |world: &mut World| -> Result {
                let system = world
                    .get_mut::<MappedSignal<T, O>>(context.entity)
                    .ok_or("expected `MappedSignalOpt`")?
                    .system
                    .take()
                    .ok_or("expected system in `MappedSignalOpt`")?;
                let id = system(world);
                world
                    .get_mut::<MappedSignal<T, O>>(context.entity)
                    .unwrap()
                    .handle = Some(id);

                Ok(())
            });

            <MappedSignal<T, O> as bevy_ecs::relationship::Relationship>::on_insert(world, context);
        }

        Option::Some(on_insert::<T, O>)
    }

    fn on_replace() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_replace)
    }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.signal.entity.map_entities(mapper);
    }
}

impl<T, O> bevy_ecs::relationship::Relationship for MappedSignal<T, O>
where
    T: Clone + Send + Sync + 'static,
    O: Bundle,
{
    type RelationshipTarget = MappedSignalComponents<T, O>;
    #[inline(always)]
    fn get(&self) -> bevy_ecs::entity::Entity {
        self.signal.entity
    }

    #[inline(always)]
    fn set_risky(&mut self, entity: Entity) {
        self.signal.entity = entity;
    }

    #[inline]
    fn from(_entity: bevy_ecs::entity::Entity) -> Self {
        panic!("cannot construct `MappedSignalOpt` from entity")
    }
}

impl<T: 'static> Signal<T> {
    pub fn map<S, O, M>(self, system: S) -> MappedSignal<T, O>
    where
        S: IntoSystem<In<T>, O, M> + Send + Sync + 'static,
        O: Bundle + 'static,
    {
        MappedSignal {
            signal: self,
            system: Some(Box::new(move |world| world.register_system(system))),
            handle: None,
        }
    }
}

#[derive(Component)]
#[relationship_target(relationship = MappedSignal<T, O>)]
pub struct MappedSignalComponents<T: Clone + Send + Sync + 'static, O: Bundle> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> (T, O)>,
}

fn apply_mapped_changes<T: Clone + Send + Sync + 'static, O: Bundle>(
    signals: Query<
        (&SignalData<T>, &MappedSignalComponents<T, O>),
        Or<(
            Changed<SignalData<Option<T>>>,
            Changed<MappedSignalComponents<T, O>>,
        )>,
    >,
    mapper: Query<&MappedSignal<T, O>>,
    mut commands: Commands,
    mut reactions: ResMut<Reactions>,
) -> Result {
    for (data, components) in &signals {
        for target in components.iter() {
            let value = data.value.read().clone();
            let mapper = mapper
                .get(target)?
                .handle
                .ok_or("expected mapper function")?;

            commands.queue(move |world: &mut World| -> Result {
                let result = world.run_system_with(mapper, value)?;
                if let Ok(mut target) = world.get_entity_mut(target) {
                    target.insert(result);
                }
                Ok(())
            });
        }

        reactions.increment();
    }

    Ok(())
}

pub struct SignalOpt<T: 'static> {
    entity: Entity,
    value: Arc<SignalInner<Option<T>>>,
}

impl<T: Send + Sync> SignalOpt<T> {
    /// Read the current value.
    ///
    /// # Panics
    ///
    /// Panics if read before the initial value is set.
    /// This will happen if the derived signal is read before
    /// its commands queue is applied.
    pub fn read(&self) -> SignalReadGuard<'_, Option<T>> {
        self.value.read()
    }
}

impl<T: Clone + Send + Sync> SignalOpt<T> {
    /// Clone the current value out of the signal.
    ///
    /// # Panics
    ///
    /// Panics if read before the initial value is set.
    /// This will happen if the derived signal is read before
    /// its commands queue is applied.
    pub fn get(&self) -> Option<T> {
        self.value.read().clone()
    }
}

#[derive(Resource, Default)]
struct RegisteredSignalOpts(HashSet<TypeId>);

impl<T> bevy_ecs::component::Component for SignalOpt<T>
where
    Self: Send + Sync + 'static,
    T: Bundle + Clone,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Immutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Bundle + Clone>(mut world: DeferredWorld, _context: HookContext) {
            if world
                .get_resource::<RegisteredSignalOpts>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<T>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredSignalOpts>()
                        .0
                        .insert(TypeId::of::<T>())
                    {
                        world
                            .resource_mut::<DeferredRegistration>()
                            .queue(|schedule| {
                                schedule.add_systems(
                                    apply_opt_changes::<T>
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

    // fn clone_behavior() -> bevy_ecs::component::ComponentCloneBehavior {
    //     use bevy_ecs::component::{DefaultCloneBehaviorBase, DefaultCloneBehaviorViaClone};
    //     (&&&bevy_ecs::component::DefaultCloneBehaviorSpecialization::<Self>::default())
    //         .default_clone_behavior()
    // }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.entity.map_entities(mapper);
    }
}

impl<T> bevy_ecs::relationship::Relationship for SignalOpt<T>
where
    T: Bundle + Clone,
{
    type RelationshipTarget = SignalOptComponents<T>;
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
        panic!("cannot construct `SignalOpt` from entity")
    }
}

impl<T> SignalOpt<T> {
    pub fn entity(&self) -> Entity {
        self.entity
    }
}

impl<T> Clone for SignalOpt<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
            entity: self.entity,
        }
    }
}

#[derive(Component)]
#[relationship_target(relationship = SignalOpt<T>)]
pub struct SignalOptComponents<T: Bundle + Clone> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> T>,
}

fn apply_opt_changes<T: Bundle + Clone>(
    signals: Query<
        (&SignalData<Option<T>>, &SignalOptComponents<T>),
        Or<(
            Changed<SignalData<Option<T>>>,
            Changed<SignalOptComponents<T>>,
        )>,
    >,
    mut commands: Commands,
) {
    for (data, components) in &signals {
        for target in components.iter() {
            match data.value.read().as_ref() {
                Some(value) => {
                    commands.entity(target).insert(value.clone());
                }
                None => {
                    commands.entity(target).remove_with_requires::<T>();
                }
            }
        }
    }
}

pub struct MappedSignalOpt<T: 'static, O> {
    signal: SignalOpt<T>,
    system: Option<Box<dyn FnOnce(&mut World) -> SystemId<In<T>, O> + Send + Sync>>,
    handle: Option<SystemId<In<T>, O>>,
}

#[derive(Resource, Default)]
struct RegisteredMappedSignalOpts(HashSet<TypeId>);

impl<T, O> bevy_ecs::component::Component for MappedSignalOpt<T, O>
where
    Self: Send + Sync + 'static,
    T: Clone + Send + Sync + 'static,
    O: Bundle,
{
    const STORAGE_TYPE: bevy_ecs::component::StorageType = bevy_ecs::component::StorageType::Table;
    type Mutability = bevy_ecs::component::Mutable;

    fn on_add() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_add<T: Clone + Send + Sync + 'static, O: Bundle>(
            mut world: DeferredWorld,
            _context: HookContext,
        ) {
            if world
                .get_resource::<RegisteredMappedSignalOpts>()
                .is_none_or(|r| !r.0.contains(&TypeId::of::<(T, O)>()))
            {
                world.commands().queue(|world: &mut World| {
                    if world
                        .get_resource_or_init::<RegisteredMappedSignalOpts>()
                        .0
                        .insert(TypeId::of::<(T, O)>())
                    {
                        world
                            .resource_mut::<DeferredRegistration>()
                            .queue(|schedule| {
                                schedule.add_systems(
                                    apply_mapped_opt_changes::<T, O>
                                        .in_set(ReactScheduleSystems::PropagateChanges),
                                );
                            });
                    }
                });
            }
        }

        Some(on_add::<T, O>)
    }

    fn on_insert() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        fn on_insert<T: Clone + Send + Sync + 'static, O: Bundle>(
            mut world: DeferredWorld,
            context: HookContext,
        ) {
            world.commands().queue(move |world: &mut World| -> Result {
                let system = world
                    .get_mut::<MappedSignalOpt<T, O>>(context.entity)
                    .ok_or("expected `MappedSignalOpt`")?
                    .system
                    .take()
                    .ok_or("expected system in `MappedSignalOpt`")?;
                let id = system(world);
                world
                    .get_mut::<MappedSignalOpt<T, O>>(context.entity)
                    .unwrap()
                    .handle = Some(id);

                Ok(())
            });

            <MappedSignalOpt<T, O> as bevy_ecs::relationship::Relationship>::on_insert(
                world, context,
            );
        }

        Option::Some(on_insert::<T, O>)
    }

    fn on_replace() -> Option<bevy_ecs::lifecycle::ComponentHook> {
        Option::Some(<Self as bevy_ecs::relationship::Relationship>::on_replace)
    }

    fn map_entities<M: bevy_ecs::entity::EntityMapper>(this: &mut Self, mapper: &mut M) {
        use bevy_ecs::entity::MapEntities;
        this.signal.entity.map_entities(mapper);
    }
}

impl<T, O> bevy_ecs::relationship::Relationship for MappedSignalOpt<T, O>
where
    T: Clone + Send + Sync + 'static,
    O: Bundle,
{
    type RelationshipTarget = MappedSignalOptComponents<T, O>;
    #[inline(always)]
    fn get(&self) -> bevy_ecs::entity::Entity {
        self.signal.entity
    }

    #[inline(always)]
    fn set_risky(&mut self, entity: Entity) {
        self.signal.entity = entity;
    }

    #[inline]
    fn from(_entity: bevy_ecs::entity::Entity) -> Self {
        panic!("cannot construct `MappedSignalOpt` from entity")
    }
}

impl<T: 'static> SignalOpt<T> {
    pub fn map<S, O, M>(self, system: S) -> MappedSignalOpt<T, O>
    where
        S: IntoSystem<In<T>, O, M> + Send + Sync + 'static,
        O: Bundle + 'static,
    {
        MappedSignalOpt {
            signal: self,
            system: Some(Box::new(move |world| world.register_system(system))),
            handle: None,
        }
    }
}

#[derive(Component)]
#[relationship_target(relationship = MappedSignalOpt<T, O>)]
pub struct MappedSignalOptComponents<T: Clone + Send + Sync + 'static, O: Bundle> {
    #[relationship]
    components: Vec<Entity>,
    marker: PhantomData<fn() -> (T, O)>,
}

fn apply_mapped_opt_changes<T: Clone + Send + Sync + 'static, O: Bundle>(
    signals: Query<
        (&SignalData<Option<T>>, &MappedSignalOptComponents<T, O>),
        Or<(
            Changed<SignalData<Option<T>>>,
            Changed<MappedSignalOptComponents<T, O>>,
        )>,
    >,
    mapper: Query<&MappedSignalOpt<T, O>>,
    mut commands: Commands,
) -> Result {
    for (data, components) in &signals {
        for target in components.iter() {
            match data.value.read().as_ref() {
                Some(value) => {
                    let mapper = mapper
                        .get(target)?
                        .handle
                        .ok_or("expected mapper function")?;
                    let value = value.clone();
                    commands.queue(move |world: &mut World| -> Result {
                        let result = world.run_system_with(mapper, value)?;
                        if let Ok(mut target) = world.get_entity_mut(target) {
                            target.insert(result);
                        }
                        Ok(())
                    });
                }
                None => {
                    commands.entity(target).remove_with_requires::<O>();
                }
            }
        }
    }

    Ok(())
}
