use apheleia_mandrake::{Cst, ParseSources, Span};
use apheleia_prism::prelude::bevy_ecs::system::Query;
use apheleia_prism::prelude::*;

#[derive(Component, EcsTreeDebug)]
pub enum Ast {
    Module {
        attributes: Vec<Entity>,
        name: String,
        items: Vec<Entity>,
    },
    Function {
        attributes: Vec<Entity>,
        name: String,
        args: Vec<Entity>,
        return_ty: Option<Entity>,
        body: Vec<Entity>,
        items: Vec<Entity>,
    },
    Argument {
        attributes: Vec<Entity>,
        name: String,
        ty: Entity,
    },
    Type {
        name: String,
    },
    RecordConstructor {
        name: String,
        fields: Vec<Entity>,
        fill: Option<Entity>,
    },
    FieldInitializer {
        name: String,
        value: Entity,
    },
    NameAccess {
        name: String,
        args: Vec<Entity>,
    },
    MemberAccess {
        inner: Entity,
        name: String,
        args: Vec<Entity>,
    },
    StringLiteral {
        string: String,
    },
    CharLiteral {
        char: char,
    },
    ExternalFunction {
        attributes: Vec<Entity>,
        name: String,
        args: Vec<Entity>,
    },
    AttributeList {
        attributes: Vec<Entity>,
    },
    Invalid {
        span: Span,
    },
}

impl Ast {
    pub fn is_scope(&self) -> bool {
        matches!(self, Ast::Module { .. } | Ast::Function { .. })
    }
}

pub struct HydraPlugin;

impl Plugin for HydraPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Main,
            (lower_cst, apply_deferred)
                .chain()
                .in_set(LowerCst)
                .after(ParseSources),
        );
    }
}

#[derive(Clone, Debug, Hash, SystemSet, PartialEq, Eq)]
pub struct LowerCst;

pub fn lower_cst(mut c: Commands, query: Query<(Entity, &Cst), Without<Ast>>) {
    for (row, cst) in query.iter() {
        let ast = lower_cst_node(cst);

        c.entity(row).insert(ast);
    }
}

pub fn lower_cst_node(cst: &Cst) -> Ast {
    match cst {
        Cst::Module {
            attributes,
            name,
            items,
            ..
        } => Ast::Module {
            attributes: attributes.clone(),
            name: name.1.clone(),
            items: items.clone(),
        },
        Cst::Function {
            attributes,
            name,
            args,
            return_ty,
            body,
            items,
            ..
        } => Ast::Function {
            attributes: attributes.clone(),
            name: name.1.clone(),
            args: args.to_vec(),
            return_ty: return_ty.as_ref().map(|(_, ty)| *ty),
            body: body.clone(),
            items: items.clone(),
        },
        Cst::FunctionArg {
            attributes,
            name,
            ty,
            ..
        } => Ast::Argument {
            attributes: attributes.clone(),
            name: name.1.clone(),
            ty: *ty,
        },
        Cst::RecordConstructor {
            name, fields, fill, ..
        } => Ast::RecordConstructor {
            name: name.1.clone(),
            fields: fields.to_vec(),
            fill: fill.as_ref().map(|(_, fill)| *fill),
        },
        Cst::FieldInitializer { name, value, .. } => Ast::FieldInitializer {
            name: name.1.clone(),
            value: *value,
        },
        Cst::NameAccess { name, args } => Ast::NameAccess {
            name: name.1.clone(),
            args: args
                .as_ref()
                .map(|(_, args, _)| args.to_vec())
                .unwrap_or_default(),
        },
        Cst::MemberAccess {
            inner, name, args, ..
        } => Ast::MemberAccess {
            inner: *inner,
            name: name.1.clone(),
            args: args
                .as_ref()
                .map(|(_, args, _)| args.to_vec())
                .unwrap_or_default(),
        },
        Cst::StringLiteral { lit } => Ast::StringLiteral {
            string: lit.1[1..lit.1.len() - 1].to_string(),
        },
        Cst::CharLiteral { lit } => Ast::CharLiteral {
            char: lit.1[1..lit.1.len() - 1].chars().next().unwrap(),
        },
        Cst::Type { name } => Ast::Type {
            name: name.1.clone(),
        },
        Cst::ExternalFunc {
            attributes,
            name,
            args,
            ..
        } => Ast::ExternalFunction {
            attributes: attributes.clone(),
            name: name.1.clone(),
            args: args.to_vec(),
        },
        Cst::AttributeList { attributes, .. } => Ast::AttributeList {
            attributes: attributes.to_vec(),
        },
        Cst::Invalid { span } => Ast::Invalid { span: span.clone() },
    }
}

#[cfg(test)]
mod test {
    use apheleia_mandrake::MandrakePlugin;
    use apheleia_prism::prelude::*;

    use crate::{Ast, HydraPlugin};

    fn lower_source(source: &str) {
        println!("Source: {source:?}");

        let mut compiler = build_compiler(&[source]);
        compiler
            .add_plugins((MandrakePlugin, HydraPlugin))
            .run_once();

        println!(
            "{:#?}",
            compiler
                .world
                .query_filtered::<Entity, With<RootNode>>()
                .single(&compiler.world)
                .component_dbg::<Ast>(&compiler.world)
        );
    }

    #[test]
    pub fn lower_putc() {
        let source = r#"
            module Main

            [FFI { source: "C", name: "putc", ..FFI.Default }]
            external fn Putc(U8)

            fn Main() {
                Putc('W'.Into)
            }
            "#;
        lower_source(source);
    }
}
