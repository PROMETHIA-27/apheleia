use std::cell::RefCell;

use bevy_ecs::entity::Entity;
use bevy_ecs::query::{QueryState, WorldQuery};
use bevy_ecs::system::CommandQueue;
use bevy_ecs::world::World;

pub type Database = World;
/// A primary key into the database storing information about the program being compiled
pub type Row = Entity;
pub use bevy_ecs::component::Component as Column;
pub use bevy_ecs::component::TableStorage;
pub type DeferQueue = CommandQueue;

thread_local! {
    pub static WORLD: RefCell<World> = RefCell::default();
}

pub trait OnColumn<Col> {
    fn on_column<Result>(self, f: impl FnOnce(&Col) -> Result) -> Result;
}

impl<Col: self::Column> OnColumn<Col> for Row {
    fn on_column<Result>(self, f: impl FnOnce(&Col) -> Result) -> Result {
        WORLD.with(|world| {
            let world = world.borrow();
            let col = world.get(self).unwrap();
            f(col)
        })
    }
}

pub trait ApplyQueue {
    fn finish(self);
}

impl ApplyQueue for DeferQueue {
    fn finish(mut self) {
        WORLD.with(|world| {
            let mut world = world.borrow_mut();
            DeferQueue::apply(&mut self, &mut world);
        })
    }
}

pub fn create_row<Col: Column>(col: Col) -> Row {
    WORLD.with(|world| world.borrow_mut().spawn(col).id())
}

pub fn insert_to_row<Col: Column>(col: Col, row: Row) -> Row {
    WORLD.with(|world| world.borrow_mut().entity_mut(row).insert(col).id())
}

pub fn insert_column<Col: Column>(col: Col, row: Row) -> impl FnOnce(&mut Database) {
    move |db| _ = db.entity_mut(row).insert(col)
}

pub struct Query<'world, Data: WorldQuery> {
    world: &'world World,
    state: QueryState<Data, ()>,
}

impl<'world, Data: WorldQuery> Query<'world, Data> {
    pub fn get(&mut self, row: Row) -> <Data::ReadOnly as WorldQuery>::Item<'world> {
        self.state.get(self.world, row).unwrap()
    }

    pub fn iter(
        &mut self,
    ) -> impl Iterator<Item = <Data::ReadOnly as WorldQuery>::Item<'world>> + '_ {
        self.state.iter(self.world)
    }
}

pub fn on_query<Data: WorldQuery, Result>(f: impl FnOnce(Query<Data>) -> Result) -> Result {
    WORLD.with(|world| {
        let mut world = world.borrow_mut();
        let state = world.query::<Data>();
        f(Query {
            world: &world,
            state,
        })
    })
}

#[macro_export]
macro_rules! derive_column {
    ($name:ident) => {
        impl ::apheleia_prism::Column for $name {
            type Storage = ::apheleia_prism::TableStorage;
        }
    };
}
