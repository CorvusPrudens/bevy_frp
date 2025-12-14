use bevy_ecs::{
    bundle::Bundle,
    component::Component,
    lifecycle::HookContext,
    world::{DeferredWorld, EntityWorldMut, World},
};

pub trait IntoAnyComponent {
    fn into_any(self) -> AnyComponent;
}

impl<B: Bundle> IntoAnyComponent for B {
    fn into_any(self) -> AnyComponent {
        AnyComponent {
            inserter: Some(Box::new(move |mut world| {
                world.insert(self);
            })),
            cleanup: |mut entity| {
                entity.remove::<B>();
            },
        }
    }
}

type AnyComponentCleanup = fn(EntityWorldMut);

#[derive(Component)]
#[component(on_insert = Self::on_insert_hook, on_replace = Self::on_replace_hook)]
pub struct AnyComponent {
    inserter: Option<Box<dyn FnOnce(EntityWorldMut) + Send + Sync>>,
    cleanup: AnyComponentCleanup,
}

impl AnyComponent {
    fn on_insert_hook(mut world: DeferredWorld, context: HookContext) {
        let inserter = world
            .get_mut::<Self>(context.entity)
            .and_then(|mut c| c.inserter.take())
            .expect("`AnyComponent` hook should access self");

        world
            .commands()
            .queue(move |world: &mut World| -> bevy_ecs::error::Result {
                let entity = world.get_entity_mut(context.entity)?;
                inserter(entity);
                Ok(())
            });
    }

    fn on_replace_hook(mut world: DeferredWorld, context: HookContext) {
        let cleanup = world
            .get::<Self>(context.entity)
            .map(|c| c.cleanup)
            .expect("`AnyComponent` hook should access self");

        world
            .commands()
            .queue(move |world: &mut World| -> bevy_ecs::error::Result {
                // The component may be despawned already
                if let Ok(entity) = world.get_entity_mut(context.entity) {
                    cleanup(entity);
                }
                Ok(())
            });
    }
}
