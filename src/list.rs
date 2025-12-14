use super::{Effect, Signal, SignalExt, Target};
use bevy_app::prelude::*;
use bevy_ecs::{
    lifecycle::HookContext, prelude::*, relationship::Relationship, system::SystemId,
    world::DeferredWorld,
};
use bevy_platform::collections::{HashMap, HashSet};
use core::marker::PhantomData;

pub struct ReactiveListPlugin;

impl Plugin for ReactiveListPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<ListTargets>();
    }
}

#[derive(Component)]
#[component(on_insert = Self::on_insert_hook, on_replace = Self::on_replace_hook)]
pub struct ReactiveList<R = ChildOf> {
    target: Target,
    _effect: Effect,
    marker: core::marker::PhantomData<fn() -> R>,
}

impl<R: 'static> ReactiveList<R> {
    fn on_insert_hook(mut world: DeferredWorld, context: HookContext) {
        let value = world
            .get::<Self>(context.entity)
            .expect("entity should have `ReactiveList` component")
            .target;

        world
            .resource_mut::<ListTargets>()
            .0
            .insert(value, context.entity);
    }

    fn on_replace_hook(mut world: DeferredWorld, context: HookContext) {
        let value = world
            .get::<Self>(context.entity)
            .expect("entity should have `ReactiveList` component")
            .target;

        world.resource_mut::<ListTargets>().0.remove(&value);
    }
}

struct ListState<I> {
    items: Vec<I>,
    set: HashSet<I>,
}

impl<I> Default for ListState<I> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            set: Default::default(),
        }
    }
}

impl<I> ListState<I>
where
    I: Eq + core::hash::Hash + Clone,
{
    pub fn diff(&mut self, mut new: Vec<I>) -> CollectionDiff<I> {
        let mut additions = Vec::new();
        let mut removals = Vec::new();

        core::mem::swap(&mut self.items, &mut new);

        let new_set: HashSet<I> = self.items.iter().cloned().collect();
        for (i, item) in self.items.iter().enumerate() {
            if !self.set.contains(item) {
                additions.push((i, item.clone()));
            }
        }

        for item in new.drain(..) {
            if !new_set.contains(&item) {
                removals.push(item);
            }
        }

        self.set = new_set;

        CollectionDiff {
            additions,
            removals,
        }
    }
}

struct CollectionDiff<I> {
    pub additions: Vec<(usize, I)>,
    pub removals: Vec<I>,
}

#[derive(Resource, Default)]
struct ListTargets(HashMap<Target, Entity>);

// attempt to reduce monomorphization ig?
pub fn reactive_list<R, I, K, B>(
    commands: &mut Commands,
    collection: Signal<Vec<I>>,
    key: Box<dyn Fn(&I) -> K + Send + Sync>,
    child: SystemId<In<I>, B>,
) -> super::ReactiveList<R>
where
    R: Relationship,
    K: Eq + core::hash::Hash + Clone + Send + Sync + 'static,
    B: Bundle,
    I: Clone + Send + Sync + 'static,
{
    let target = Target::new();
    let mut state = ListState::default();
    let mut entities = HashMap::<K, Entity>::default();
    let mut queued_changes = Vec::new();

    // We implicitly memoize here so the list effect only
    // runs when our specific target changes.
    let target_sig =
        commands.derive(move |targets: Res<ListTargets>| targets.0.get(&target).copied());

    let effect = commands.effect(move |mut commands: Commands| {
        let collection = collection.read();
        let new_keys = collection.iter().map(&key).collect();

        let CollectionDiff {
            additions,
            removals,
        } = state.diff(new_keys);

        for removal in removals {
            if let Some(entity) = entities.remove(&removal) {
                if let Ok(mut entity) = commands.get_entity(entity) {
                    entity.try_despawn();
                }
            }
        }

        for (i, key) in additions {
            let new_entity = commands.spawn_empty().id();
            entities.insert(key, new_entity);

            // TODO: fix the part where items could have been removed before these
            // are applied.
            queued_changes.push((new_entity, collection[i].clone()));
        }

        if let Some(target) = target_sig.get() {
            for (new_entity, system_input) in queued_changes.drain(..) {
                commands.queue(move |world: &mut World| -> Result {
                    let result = world.run_system_with(child, system_input)?;
                    world
                        .get_entity_mut(new_entity)?
                        .insert((result, R::from(target)));

                    Ok(())
                });
            }
        }
    });

    super::ReactiveList {
        target,
        _effect: effect,
        marker: PhantomData,
    }
}
