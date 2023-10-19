//! This module contains machinery related to traversing trees (and perhaps graphs) in the ECS
//! in an ergonomic way. It exists because rather than using a single enum to represent a given
//! tree (for example `enum Ast { Module { ... }, FunctionDef { ... } }`), apheleia uses individual
//! structs which are shared between multiple trees. A tree's definition then specifies a set of
//! node types that it is composed of; for example `Ast { Module, FunctionDef, ... }`.
//!
//! Then, nodes are created by `.spawn()`ing them into the [`World`], and inserting both a tree
//! discriminator (the fieldless enum) and a node into it. This could be difficult to traverse,
//! because so many types are involved. Thus this module contains helpers that make traversal
//! easier (as well as macros from [`apheleia_macro`]).

use bevy_ecs::entity::Entity;
use bevy_ecs::world::World;

/// It is often useful to be able to generically perform an action on every child of a node in a
/// tree. This node does that, calling `f` on every [`Entity`] contained within a given tree node.
///
/// It is implemented via derive macro.
pub trait Visit {
    fn visit(&self, f: impl FnMut(Entity));
}

/// Normally when traversing a tree you can pass a closure to invoke on every node. However,
/// this only works when either every node is the same type, or you can pass generic closures which
/// dispatch on the type of their parameters. The former does not hold, and rust does not have generic
/// closures, so instead we need to pass the operation at the type level. A type is created and has
/// this trait implemented to represent the operation itself, and then [`Operate<Op>`] is implemented
/// for each node to specify what the operation does to that node.
pub trait Operation {
    /// Operations generally take additional arguments that are different for each operation,
    /// so this generic associated type allows an operation to take additional parameters.
    ///
    /// It's generic in case the user wants to take a reference. `'args` can be used to reference
    /// the overall type, `'world` can be used to reference anything borrowed from the `&world` passed
    /// around (including the nodes themselves), and `'other` is useful for any other borrows
    /// that last the entire operation.
    type Arguments<'args, 'world: 'args, 'other: 'args>;
}

/// See [`Operation`].
pub trait Operate<Op: Operation> {
    fn operate<'args, 'world: 'args, 'rest: 'args>(
        &'world self,
        entity: Entity,
        world: &'world World,
        args: Op::Arguments<'args, 'world, 'rest>,
    );
}

/// In the process of traversing a heterogenous tree, there needs to be a step where the type of
/// a node is identified and then the operation can actually be called for that type. This function
/// ([`Dispatch::dispatch_impl()`]) usually looks like:
/// ```rust
/// let kind = world.get::<Tree>(entity).unwrap();
/// match kind {
///     Tree::Node1 => {
///         let node = world.get::<Node1>(entity).unwrap();
///         node.operate(entity, world, args);
///     },
///     ...
/// }
/// ```
///
/// It is implemented via derive macro.
pub trait Dispatch<Op: Operation> {
    fn dispatch_impl<'args, 'world: 'args, 'rest: 'args>(
        entity: Entity,
        world: &'world World,
        args: Op::Arguments<'args, 'world, 'rest>,
    );
}

/// This trait makes it possible to invoke dispatch like `Tree::dispatch::<Op>(...)`, because
/// the actual trait must have the call signature `<Tree as Dispatch<Op>>::dispatch(...)`, which is
/// ugly and verbose.
pub trait DispatchHelper {
    fn dispatch<'args, 'world: 'args, 'rest: 'args, Op: Operation>(
        entity: Entity,
        world: &'world World,
        args: Op::Arguments<'args, 'world, 'rest>,
    ) where
        Self: Dispatch<Op>;
}

impl<T> DispatchHelper for T {
    #[inline(always)]
    fn dispatch<'args, 'world: 'args, 'rest: 'args, Op: Operation>(
        entity: Entity,
        world: &'world World,
        args: Op::Arguments<'args, 'world, 'rest>,
    ) where
        Self: Dispatch<Op>,
    {
        T::dispatch_impl(entity, world, args)
    }
}
