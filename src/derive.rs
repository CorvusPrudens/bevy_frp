use bevy_ecs::{
    prelude::*,
    query::{QueryData, QueryFilter},
};
use variadics_please::all_tuples;

/// Compute whether a system param may have changed.
///
/// This trait is permissive; system params that can't
/// effectively compute whether they've changed always
/// return `true`.
pub trait MaybeChanged {
    fn maybe_changed(world: &World) -> bool;
}

impl<T: Resource> MaybeChanged for Res<'_, T> {
    fn maybe_changed(world: &World) -> bool {
        world
            .get_resource_ref::<T>()
            .is_some_and(|r| r.is_changed())
    }
}

// TODO: this isn't quite right
impl<T: Resource> MaybeChanged for Option<Res<'_, T>> {
    fn maybe_changed(world: &World) -> bool {
        world
            .get_resource_ref::<T>()
            .is_some_and(|r| r.is_changed())
    }
}

impl<D, F> MaybeChanged for Query<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    fn maybe_changed(_: &World) -> bool {
        true
    }
}

impl<D, F> MaybeChanged for Single<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    fn maybe_changed(_: &World) -> bool {
        true
    }
}

impl<D, F> MaybeChanged for super::target::TQuery<'_, '_, D, F>
where
    D: QueryData,
    F: QueryFilter,
{
    fn maybe_changed(_: &World) -> bool {
        true
    }
}

impl MaybeChanged for () {
    fn maybe_changed(_world: &World) -> bool {
        false
    }
}

macro_rules! maybe_changed {
    ($($ty:ident),*) => {
        impl<$($ty),*> MaybeChanged for ($($ty,)*)
            where $($ty: MaybeChanged),*
        {
            fn maybe_changed(world: &World) -> bool {
                ($(
                    $ty::maybe_changed(world)
                )||*)
            }
        }
    };
}

all_tuples!(maybe_changed, 1, 15, T);

/// Compute whether a system param has definitely changed.
///
/// This trait is restricted to system params that have
/// trivial change detection.
pub trait IsChanged {
    fn is_changed(world: &World) -> bool;
}

impl<T: Resource> IsChanged for Res<'_, T> {
    fn is_changed(world: &World) -> bool {
        world
            .get_resource_ref::<T>()
            .is_some_and(|r| r.is_changed())
    }
}

impl IsChanged for () {
    fn is_changed(_world: &World) -> bool {
        false
    }
}

impl IsChanged for Commands<'_, '_> {
    fn is_changed(_world: &World) -> bool {
        false
    }
}

macro_rules! is_changed {
    ($($ty:ident),*) => {
        impl<$($ty),*> IsChanged for ($($ty,)*)
            where $($ty: IsChanged),*
        {
            fn is_changed(world: &World) -> bool {
                ($(
                    $ty::is_changed(world)
                )||*)
            }
        }
    };
}

all_tuples!(is_changed, 1, 15, T);
