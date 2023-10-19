use std::collections::BTreeMap;
use std::marker::PhantomData;
use std::path::PathBuf;

use bevy_app::App;
use bevy_ecs::component::Component;
use bevy_ecs::entity::Entity;
use bevy_ecs::system::Resource;
use bevy_ecs::world::World;

pub mod prelude {
    pub use apheleia_macro::{impl_for_all, Dispatch, EcsTreeDebug, Visit};

    pub use bevy_ecs;
    pub use bevy_ecs::prelude::*;

    pub use bevy_app;
    pub use bevy_app::prelude::*;

    pub use super::traversal::{Dispatch, DispatchHelper, Operate, Operation, Visit};
    pub use super::{
        build_compiler, DebugAsEcsTree, EcsTreeDebug, RootNode, RunAppOnce, SourceFiles,
    };
}

mod traversal;

pub fn build_compiler(sources: &[&str]) -> App {
    let mut app = App::empty();
    app.insert_resource(SourceFiles {
        files: sources
            .iter()
            .map(|src| (PathBuf::new(), src.to_string()))
            .collect(),
    });
    app
}

pub trait RunAppOnce {
    /// [`bevy_ecs`]'s default behavior when running the [`App`] is to consume the [`World`] inside.
    /// This is inconvenient when trying to use what is left in the world after running, so this
    /// function will run the app's schedule once *without* consuming the world.
    fn run_once(&mut self);
}

impl RunAppOnce for App {
    fn run_once(&mut self) {
        while !self.ready() {
            #[cfg(not(target_arch = "wasm32"))]
            bevy_tasks::tick_global_task_pools_on_main_thread();
        }
        self.finish();
        self.cleanup();

        self.update();
    }
}

#[derive(Resource)]
pub struct SourceFiles {
    pub files: BTreeMap<PathBuf, String>,
}

#[derive(Component)]
pub struct RootNode;

pub struct DebugAsEcsTree<'w, T, C>(&'w World, T, PhantomData<C>);

impl<'w, T, C> std::fmt::Debug for DebugAsEcsTree<'w, T, C>
where
    T: EcsTreeDebug,
    C: Component + EcsTreeDebug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.fmt::<C>(self.0, f)
    }
}

pub trait EcsTreeDebug {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug;

    fn component_dbg<'w, C>(&self, world: &'w World) -> DebugAsEcsTree<'w, &Self, C> {
        DebugAsEcsTree(world, self, PhantomData)
    }
}

impl<T: EcsTreeDebug> EcsTreeDebug for &T {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        T::fmt::<C>(self, world, f)
    }
}

impl EcsTreeDebug for Entity {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        world.get::<C>(*self).unwrap().fmt::<C>(world, f)
    }
}

impl EcsTreeDebug for char {
    fn fmt<Col>(&self, _: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        Col: Component + EcsTreeDebug,
    {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl EcsTreeDebug for String {
    fn fmt<Col>(&self, _: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        Col: Component + EcsTreeDebug,
    {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl<T: EcsTreeDebug> EcsTreeDebug for Vec<T> {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        let mut list = f.debug_list();
        for item in self {
            list.entry(&item.component_dbg::<C>(world));
        }
        list.finish()
    }
}

impl EcsTreeDebug for std::ops::Range<usize> {
    fn fmt<C>(&self, _: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl<T: EcsTreeDebug, U: EcsTreeDebug> EcsTreeDebug for (T, U) {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        f.write_fmt(format_args!(
            "({:?}, {:?})",
            &self.0.component_dbg::<C>(world),
            &self.1.component_dbg::<C>(world)
        ))
    }
}

impl<T: EcsTreeDebug, U: EcsTreeDebug, V: EcsTreeDebug> EcsTreeDebug for (T, U, V) {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        f.write_fmt(format_args!(
            "({:?}, {:?}, {:?})",
            &self.0.component_dbg::<C>(world),
            &self.1.component_dbg::<C>(world),
            &self.2.component_dbg::<C>(world),
        ))
    }
}

impl<T: EcsTreeDebug> EcsTreeDebug for Option<T> {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        match self {
            Some(value) => f
                .debug_tuple("Some")
                .field(&value.component_dbg::<C>(world))
                .finish(),
            None => f.write_str("None"),
        }
    }
}
