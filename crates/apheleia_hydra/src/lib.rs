use apheleia_bestiary::BuiltinTypes;
use apheleia_mandrake::{
    AttributeList, CharLiteral, FieldInitializer, Invalid, Module, StringLiteral,
};
use apheleia_narcissus::{
    FunctionCall, Hir, TypedExternalFunc, TypedFunctionArg, TypedFunctionDef,
    TypedRecordConstructor,
};
use apheleia_prism::prelude::*;

#[derive(Component)]
pub struct ExprOfType {
    pub ty: Entity,
}

struct TypeckHir;

impl Operation for TypeckHir {
    type Arguments<'args, 'world: 'args, 'x: 'args> = &'args mut TypeckArgs<'world, 'x>;
}

struct TypeckArgs<'world, 'x> {
    commands: Commands<'x, 'x>,
    builtins: &'world BuiltinTypes,
}

impl Operate<TypeckHir> for StringLiteral {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        entity: Entity,
        _: &'world World,
        args: &mut TypeckArgs,
    ) {
        args.commands.entity(entity).insert(ExprOfType {
            ty: *args.builtins.types.get("String").unwrap(),
        });
    }
}

impl Operate<TypeckHir> for CharLiteral {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        entity: Entity,
        _: &'world World,
        args: &mut TypeckArgs,
    ) {
        args.commands.entity(entity).insert(ExprOfType {
            ty: *args.builtins.types.get("Char").unwrap(),
        });
    }
}

#[impl_for_all(
    Module,
    TypedFunctionDef,
    TypedFunctionArg,
    TypedRecordConstructor,
    FieldInitializer,
    FunctionCall,
    TypedExternalFunc,
    AttributeList,
    Invalid
)]
impl Operate<TypeckHir> for This {
    fn operate<'args, 'world: 'args, 'x: 'args>(
        &'world self,
        _: Entity,
        world: &'world World,
        args: &mut TypeckArgs<'world, 'x>,
    ) {
        self.visit(|child| Hir::dispatch::<TypeckHir>(child, world, args))
    }
}
