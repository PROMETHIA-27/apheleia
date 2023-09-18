use std::collections::HashMap;

use apheleia_hydra::{Ast, LowerCst};
use apheleia_prism::prelude::*;

#[derive(Component)]
pub struct Namespace(String);

impl Namespace {
    pub fn from_parts(parts: &[&str]) -> Self {
        let mut str = parts.iter().fold(String::new(), |mut accum, part| {
            accum.push_str(part);
            accum.push('.');
            accum
        });
        str.remove(str.len() - 1);
        Self(str)
    }
}

/// The namespace and name that uniquely identifies an item. Modules do not have qualified names,
/// they are namespaces themselves.
#[derive(Component)]
pub struct QualifiedName {
    pub namespace: String,
    pub name: String,
}

#[derive(Component)]
pub enum Item {
    Function,
    ExternalFunction,
}

#[derive(Component, Debug, Default)]
pub struct VisibleItems {
    pub items: HashMap<String, Entity>,
}

#[derive(Component)]
pub struct ParentScope(Entity);

ir_tree! {
    #[from(Ast)]
    #[derive(Component, EcsTreeDebug)]
    pub enum Hir {
        unchanged {
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
        }
        new {
            FunctionCall {
                function: Entity,
                args: Vec<Entity>,
            },
        }
    }
}

pub struct NarcissusPlugin;

impl Plugin for NarcissusPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Main,
            (
                walk_declarations,
                apply_deferred,
                build_visible_items,
                apply_deferred,
                lower_ast,
                apply_deferred,
            )
                .chain()
                .in_set(LowerAst)
                .after(LowerCst),
        );
    }
}

#[derive(Clone, Debug, Hash, SystemSet, PartialEq, Eq)]
pub struct LowerAst;

pub fn lower_ast(
    mut c: Commands,
    roots: Query<Entity, With<RootNode>>,
    asts: Query<&Ast>,
    visible_items: Query<&VisibleItems>,
) {
    for root in &roots {
        lower_ast_node(root, &asts, &visible_items, None, &mut c);
    }

    fn lower_ast_node(
        ast: Entity,
        asts: &Query<&Ast>,
        visible_items: &Query<&VisibleItems>,
        scope: Option<Entity>,
        c: &mut Commands,
    ) {
        match asts.get(ast).unwrap() {
            node @ Ast::Module {
                attributes, items, ..
            } => {
                for &attr in attributes {
                    lower_ast_node(attr, asts, visible_items, scope, c);
                }
                for &item in items {
                    lower_ast_node(item, asts, visible_items, Some(ast), c);
                }
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::Function {
                attributes,
                args,
                body,
                items,
                return_ty,
                ..
            } => {
                for &attr in attributes {
                    lower_ast_node(attr, asts, visible_items, scope, c);
                }
                for &arg in args {
                    lower_ast_node(arg, asts, visible_items, scope, c);
                }
                return_ty.map(|ty| lower_ast_node(ty, asts, visible_items, scope, c));
                for &item in items {
                    lower_ast_node(item, asts, visible_items, Some(ast), c);
                }
                for &expr in body {
                    lower_ast_node(expr, asts, visible_items, Some(ast), c);
                }
                c.entity(ast).insert(Hir::from(node));
            }
            function_call @ (Ast::NameAccess { .. } | Ast::MemberAccess { .. }) => {
                let (name, args) = match function_call {
                    Ast::NameAccess { name, args, .. } => (name, args.clone()),
                    Ast::MemberAccess {
                        name, inner, args, ..
                    } => (name, [inner].into_iter().chain(args).copied().collect()),
                    _ => unreachable!(),
                };
                let scope_items = visible_items.get(scope.unwrap()).unwrap();
                // TODO: This unwrap is an assertion that all names are in scope
                let function = *scope_items.items.get(&name[..]).unwrap();
                c.entity(ast).insert(Hir::FunctionCall { function, args });
                match function_call {
                    Ast::NameAccess { args, .. } => {
                        for &ast in args {
                            lower_ast_node(ast, asts, visible_items, scope, c);
                        }
                    }
                    Ast::MemberAccess { inner, args, .. } => {
                        for &ast in args {
                            lower_ast_node(ast, asts, visible_items, scope, c);
                        }
                        lower_ast_node(*inner, asts, visible_items, scope, c);
                    }
                    _ => unreachable!(),
                }
            }
            node @ Ast::Argument { attributes, ty, .. } => {
                for &ast in attributes {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                lower_ast_node(*ty, asts, visible_items, scope, c);
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::Type { .. } => _ = c.entity(ast).insert(Hir::from(node)),
            node @ Ast::RecordConstructor { fields, fill, .. } => {
                for &ast in fields {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                for &ast in fill {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::FieldInitializer { value, .. } => {
                lower_ast_node(*value, asts, visible_items, scope, c);
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::StringLiteral { .. } => {
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::CharLiteral { .. } => {
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::ExternalFunction {
                attributes, args, ..
            } => {
                for &ast in attributes {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                for &ast in args {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                c.entity(ast).insert(Hir::from(node));
            }
            node @ Ast::AttributeList { attributes } => {
                for &ast in attributes {
                    lower_ast_node(ast, asts, visible_items, scope, c);
                }
                c.entity(ast).insert(Hir::from(node));
            }
            Ast::Invalid { .. } => todo!(),
        };
    }
}

pub fn walk_declarations(
    mut c: Commands,
    roots: Query<Entity, With<RootNode>>,
    asts: Query<&Ast>,
    world: &World,
) {
    let scope_names = &mut vec![];
    let scopes = &mut vec![];

    for ast in &roots {
        walk_declarations_impl(&asts, ast, scope_names, scopes, &mut c, world);
    }

    fn walk_declarations_impl<'ast>(
        asts: &'ast Query<&Ast>,
        ast: Entity,
        scope_names: &mut Vec<&'ast str>,
        scopes: &mut Vec<Entity>,
        c: &mut Commands,
        world: &World,
    ) {
        match asts.get(ast).unwrap() {
            Ast::Module {
                name,
                items,
                ..
            } => {
                scope_names.push(name);
                scopes.push(ast);
                c.entity(ast).insert(Namespace::from_parts(scope_names));
                if let Some(scope) = scopes.last() {
                    c.entity(ast).insert(ParentScope(*scope));
                }
                for &ast in items {
                    walk_declarations_impl(asts, ast, scope_names, scopes, c, world);
                }
                scopes.pop();
                scope_names.pop();
            }
            Ast::Function {
                name,
                items,
                ..
            } => {
                c.entity(ast).insert(Item::Function);
                let namespace = Namespace::from_parts(scope_names);
                c.entity(ast).insert((QualifiedName {
                    namespace: namespace.0.clone(),
                    name: name.to_string(),
                }, namespace, ParentScope(*scopes.last().unwrap())));
                scope_names.push(name);
                scopes.push(ast);
                for &ast in items {
                    walk_declarations_impl(asts, ast, scope_names, scopes, c, world);
                }
                scope_names.pop();
                scopes.pop();
            }
            Ast::ExternalFunction {
                name, ..
            } => {
                c.entity(ast).insert((
                    Item::ExternalFunction,
                    ParentScope(*scopes.last().unwrap()),
                    QualifiedName {
                        namespace: Namespace::from_parts(scope_names).0,
                        name: name.to_string(),
                    }
                ));
            }
            kind => unreachable!(
                "internal compiler invariant violated; attempted to walk ast node of kind {:?} during declaration collection",
                kind.component_dbg::<Ast>(world)
            ),
        }
    }
}

pub fn build_visible_items(
    mut c: Commands,
    roots: Query<Entity, With<RootNode>>,
    asts: Query<&Ast>,
    parents: Query<&ParentScope>,
    names: Query<&QualifiedName>,
) {
    for ast in &roots {
        build_visible_items_impl(&mut c, ast, &asts, &parents, &names);
    }

    fn build_visible_items_impl(
        c: &mut Commands,
        ast: Entity,
        asts: &Query<&Ast>,
        parents: &Query<&ParentScope>,
        names: &Query<&QualifiedName>,
    ) {
        let mut vis = VisibleItems::default();

        match asts.get(ast).unwrap() {
                Ast::Module { items, name, .. } => {
                        if let Ok(&ParentScope(parent)) = parents.get(ast) {
                            match asts.get(parent).unwrap() {
                                Ast::Module { items, .. } | Ast::Function { items, .. } => {
                                    for &item in items {
                                        let name = names.get(item).unwrap().name.clone();
                                        vis.items.insert(name, item);
                                    }
                                },
                                _ => unreachable!("internal compiler invariant violated: parent scope was not a scope"),
                            }
                        }
                    vis.items.insert(name.to_string(), ast);
                    for &item in items.iter().filter(|&&item| asts.get(item).unwrap().is_scope()) {
                        let name = names.get(item).unwrap().name.clone();
                        vis.items.insert(name, item);
                        build_visible_items_impl(c,item, asts, parents, names);
                    }
                }
                 Ast::Function { items, name, .. } => {
                        if let Ok(&ParentScope(parent)) = parents.get(ast) {
                            match asts.get(parent).unwrap() {
                                Ast::Module { items, .. } | Ast::Function { items, .. } => {
                                    for &item in items {
                                        let name = names.get(item).unwrap().name.clone();
                                        vis.items.insert(name, item);
                                    }
                                },
                                _ => unreachable!("internal compiler invariant violated: parent scope was not a scope"),
                            }
                        }

                    vis.items.insert(name.to_string(), ast);
                    for &item in items.iter().filter(|&&item| asts.get(item).unwrap().is_scope()) {
                        let name = names.get(item).unwrap().name.clone();
                        vis.items.insert(name, item);
                        build_visible_items_impl(c,item, asts, parents, names);
                    }
                 }
                Ast::ExternalFunction { .. } => {},
                _ => unreachable!("internal compiler invariant violated, attempted to build visible items for a non-scope node"),
            }

        c.entity(ast).insert(vis);
    }
}

#[cfg(test)]
mod test {
    use apheleia_hydra::HydraPlugin;
    use apheleia_mandrake::MandrakePlugin;
    use apheleia_prism::prelude::*;

    use crate::{Hir, NarcissusPlugin};

    fn lower_source(source: &str) {
        let mut compiler = build_compiler(&[source]);
        compiler
            .add_plugins((MandrakePlugin, HydraPlugin, NarcissusPlugin))
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

    #[test]
    fn cross_file_function_call() {
        let source1 = r#"
            module One

            fn Foo() {}
        "#;
        let source2 = r#"
            module Two

            fn Bar() {
                Foo()
            }
        "#;

        let mut compiler = build_compiler(&[source1, source2]);
        compiler
            .add_plugins((MandrakePlugin, HydraPlugin, NarcissusPlugin))
            .run_once();

        for root in compiler
            .world
            .query_filtered::<Entity, With<RootNode>>()
            .iter(&compiler.world)
        {
            println!("{:#?}", root.component_dbg::<Hir>(&compiler.world));
        }
    }

    #[test]
    fn lower_putc() {
        lower_source(
            r#"
            module Main

            fn FFI() {}

            fn Default() -> FFI {}

            [FFI { source: "C", name: "putc", ..FFI.Default }]
            external fn Putc(U8)

            fn TryInto(char: Char) -> OptionU8 {}

            fn Unwrap(option: OptionU8) -> U8 {}

            fn Main() {
                Putc('W'.TryInto.Unwrap)
            }
            "#,
        );
    }
}
