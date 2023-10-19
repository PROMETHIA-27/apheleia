use apheleia_bestiary::QualifiedName;
use apheleia_mandrake::{
    AttributeList, CharLiteral, Cst, ExternalFunc, FieldInitializer, FunctionArg, FunctionDef,
    Invalid, MemberAccess, Module, NameAccess, RecordConstructor, StringLiteral, Type,
};
use apheleia_prism::prelude::*;

use crate::Item;

/// While doing name resolution it's useful to know what the full path of items are, so this pass
/// annotates items with a [`QualifiedName`] and some other information.
pub fn annotate_items(c: Commands, world: &World, roots: Query<Entity, With<RootNode>>) {
    let mut args = AnnotateArgs {
        commands: c,
        stack: vec![],
    };
    for root in &roots {
        Cst::dispatch::<AnnotateItems>(root, world, &mut args);
    }
}

struct AnnotateItems;

impl Operation for AnnotateItems {
    type Arguments<'args, 'world: 'args, 'x: 'args> = &'args mut AnnotateArgs<'world, 'x>;
}

struct AnnotateArgs<'world, 'x> {
    commands: Commands<'x, 'x>,
    stack: Vec<&'world str>,
}

impl AnnotateArgs<'_, '_> {
    fn get_current_namespace(&self) -> String {
        let mut namespace = self
            .stack
            .iter()
            .fold(String::new(), |accum, next| accum + next + ".");
        namespace.pop();
        namespace
    }
}

impl Operate<AnnotateItems> for Module {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        _: Entity,
        world: &'world World,
        args: &'args mut AnnotateArgs<'world, 'x>,
    ) {
        args.stack.push(&self.name.1);
        for &item in &self.items {
            Cst::dispatch::<AnnotateItems>(item, world, args);
        }
        args.stack.pop();
    }
}

impl Operate<AnnotateItems> for FunctionDef {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        entity: Entity,
        world: &'world World,
        args: &'args mut AnnotateArgs<'world, 'x>,
    ) {
        let namespace = args.get_current_namespace();
        args.commands.entity(entity).insert((
            Item::Function,
            QualifiedName {
                namespace,
                name: self.name.1.clone(),
            },
        ));
        args.stack.push(&self.name.1);
        for &item in &self.items {
            Cst::dispatch::<AnnotateItems>(item, world, args);
        }
        args.stack.pop();
    }
}

impl Operate<AnnotateItems> for ExternalFunc {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        entity: Entity,
        _: &'world World,
        args: &'args mut AnnotateArgs<'world, 'x>,
    ) {
        let namespace = args.get_current_namespace();
        args.commands.entity(entity).insert((
            Item::Function,
            QualifiedName {
                namespace,
                name: self.name.1.clone(),
            },
        ));
    }
}

// TODO: Blocks will probably be allowed to contain items in the future, so this needs to be changed
// to visit all nodes, not just top-level items.
#[impl_for_all(
    FunctionArg,
    RecordConstructor,
    FieldInitializer,
    NameAccess,
    MemberAccess,
    StringLiteral,
    CharLiteral,
    Type,
    AttributeList,
    Invalid
)]
impl Operate<AnnotateItems> for This {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        entity: Entity,
        world: &'world World,
        _: &'args mut AnnotateArgs<'world, 'x>,
    ) {
        unreachable!(
        "internal compiler invariant violated; attempted to walk cst node of kind {:?} during annotation",
        entity.component_dbg::<Cst>(world)
    )
    }
}
