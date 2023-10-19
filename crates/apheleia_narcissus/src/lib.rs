use annotate::annotate_items;
use apheleia_mandrake::{
    AttributeList, CharLiteral, FieldInitializer, Invalid, Module, ParseSources, StringLiteral,
};
use apheleia_prism::prelude::*;
use lower::lower_cst;

mod annotate;
mod lower;

#[derive(Component)]
pub enum Item {
    Function,
    ExternalFunction,
}

#[derive(Component, Debug, Visit)]
pub struct TypedFunctionDef {
    /// any attached attribute lists
    pub attributes: Vec<Entity>,
    /// the identifier of the function
    pub name: String,
    /// the arguments
    pub args: Vec<Entity>,
    /// the `->` token and return type, if any
    pub return_ty: Entity,
    /// the contents of the function
    pub body: Vec<Entity>,
    /// any items declared in the body of the function
    pub items: Vec<Entity>,
}

#[derive(Component, Debug, Visit)]
pub struct TypedFunctionArg {
    /// the attributes of the argument
    pub attributes: Vec<Entity>,
    /// the identifier of the argument
    pub name: String,
    /// the type of the argument
    pub ty: Entity,
}

#[derive(Component, Debug, Visit)]
pub struct TypedRecordConstructor {
    /// the type to construct
    pub ty: Entity,
    /// the field initializers
    pub fields: Vec<Entity>,
    /// the `..expr` part of the declaration; declares that this constructor is actually overwriting
    /// the value given by the expression. The first span is the `..` token.
    pub fill: Option<Entity>,
}

#[derive(Component, Debug, Visit)]
pub struct FunctionCall {
    function: Entity,
    args: Vec<Entity>,
}

#[derive(Component, Debug, Visit)]
pub struct TypedExternalFunc {
    /// any attached attribute lists
    pub attributes: Vec<Entity>,
    /// the identifier of the function
    pub name: String,
    /// the argument types
    pub args: Vec<Entity>,
}

#[derive(Copy, Clone, Component, Debug, Dispatch, EcsTreeDebug)]
pub enum Hir {
    Module,
    TypedFunctionDef,
    TypedFunctionArg,
    TypedRecordConstructor,
    FieldInitializer,
    FunctionCall,
    StringLiteral,
    CharLiteral,
    TypedExternalFunc,
    AttributeList,
    Invalid,
}

pub struct NarcissusPlugin;

impl Plugin for NarcissusPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Main,
            (annotate_items, apply_deferred, lower_cst, apply_deferred)
                .chain()
                .in_set(LowerAst)
                .after(ParseSources),
        );
    }
}

#[derive(Clone, Debug, Hash, SystemSet, PartialEq, Eq)]
pub struct LowerAst;

#[cfg(test)]
mod test {
    use apheleia_mandrake::MandrakePlugin;
    use apheleia_prism::prelude::*;

    use crate::{Hir, NarcissusPlugin};

    fn lower_source(source: &str) {
        let mut compiler = build_compiler(&[source]);
        compiler
            .add_plugins((MandrakePlugin, NarcissusPlugin))
            .run_once();

        println!(
            "{:#?}",
            compiler
                .world
                .query_filtered::<Entity, With<RootNode>>()
                .single(&compiler.world)
                .component_dbg::<Hir>(&compiler.world)
        );
    }

    // #[test]
    // fn cross_file_function_call() {
    //     let source1 = r#"
    //         module One

    //         fn Foo() {}
    //     "#;
    //     let source2 = r#"
    //         module Two

    //         fn Bar() {
    //             Foo()
    //         }
    //     "#;

    //     let mut compiler = build_compiler(&[source1, source2]);
    //     compiler
    //         .add_plugins((MandrakePlugin, NarcissusPlugin))
    //         .run_once();

    //     for root in compiler
    //         .world
    //         .query_filtered::<Entity, With<RootNode>>()
    //         .iter(&compiler.world)
    //     {
    //         println!("{:#?}", root.component_dbg::<Hir>(&compiler.world));
    //     }
    // }

    #[test]
    fn lower_putc() {
        lower_source(
            r#"
            module Main

            external fn Putc(Char)

            fn Main() {
                Putc('W')
            }
            "#,
        );
    }
}
