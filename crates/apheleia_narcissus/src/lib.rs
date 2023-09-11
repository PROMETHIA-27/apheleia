use std::collections::HashMap;

use apheleia_hydra::Ast;
use apheleia_prism::{
    derive_column, insert_column, ir_tree, on_query, ApplyQueue, ColumnDebug, Database, DeferQueue,
    Query, QueryState, Row, WORLD,
};

derive_column!(Namespace);
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

derive_column!(QualifiedName);
pub struct QualifiedName {
    pub namespace: String,
    pub name: String,
}

derive_column!(Item);
pub enum Item {
    Function,
    ExternalFunction,
}

derive_column!(VisibleItems);
#[derive(Debug, Default)]
pub struct VisibleItems {
    pub items: HashMap<String, Row>,
}

derive_column!(ParentScope);
pub struct ParentScope(Row);

derive_column!(Hir);
ir_tree! {
    #[from(Ast)]
    #[derive(ColumnDebug)]
    pub enum Hir {
        unchanged {
            Module {
                attributes: Vec<Row>,
                name: String,
                items: Vec<Row>,
            },
            Function {
                attributes: Vec<Row>,
                name: String,
                args: Vec<Row>,
                return_ty: Option<Row>,
                body: Vec<Row>,
                items: Vec<Row>,
            },
            Argument {
                attributes: Vec<Row>,
                name: String,
                ty: Row,
            },
            Type {
                name: String,
            },
            RecordConstructor {
                name: String,
                fields: Vec<Row>,
                fill: Option<Row>,
            },
            FieldInitializer {
                name: String,
                value: Row,
            },
            StringLiteral {
                string: String,
            },
            CharLiteral {
                char: char,
            },
            ExternalFunction {
                attributes: Vec<Row>,
                name: String,
                args: Vec<Row>,
            },
            AttributeList {
                attributes: Vec<Row>,
            },
        }
        new {
            FunctionCall {
                function: Row,
                args: Vec<Row>,
            },
        }
    }
}

pub fn lower_ast(roots: &[Row]) {
    attach_declarations(roots);

    for &root in roots {
        let mut queue = DeferQueue::default();
        WORLD.with(|db| {
            let mut db = db.borrow_mut();
            let mut asts = db.query::<&Ast>();
            let mut parents = db.query::<&ParentScope>();
            let mut visible_items = db.query::<&VisibleItems>();

            let mut stack = vec![StackFrame::Init { ast: root }];
            let mut scope = None;

            while let Some(frame) = stack.pop() {
                lower_ast_node(
                    frame,
                    &mut asts,
                    &mut parents,
                    &mut visible_items,
                    &db,
                    &mut scope,
                    &mut stack,
                    &mut queue,
                );
            }
        });
        queue.finish();
    }

    // Helpers

    enum StackFrame {
        Init { ast: Row },
        PopScope { old_scope: Option<Row> },
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_ast_node(
        frame: StackFrame,
        asts: &mut QueryState<&Ast>,
        parents: &mut QueryState<&ParentScope>,
        visible_items: &mut QueryState<&VisibleItems>,
        db: &Database,
        scope: &mut Option<Row>,
        stack: &mut Vec<StackFrame>,
        queue: &mut DeferQueue,
    ) {
        match frame {
            StackFrame::Init { ast } => match asts.get(db, ast).unwrap() {
                scope_ast @ (Ast::Module { .. } | Ast::Function { .. }) => {
                    let old_scope = *scope;
                    *scope = Some(ast);
                    match scope_ast {
                        Ast::Module {
                            attributes, items, ..
                        } => {
                            for &ast in attributes {
                                stack.push(StackFrame::Init { ast });
                            }
                            stack.push(StackFrame::PopScope { old_scope });
                            for &ast in items {
                                stack.push(StackFrame::Init { ast });
                            }
                        }
                        Ast::Function {
                            attributes,
                            args,
                            body,
                            items,
                            return_ty,
                            ..
                        } => {
                            for &ast in attributes {
                                stack.push(StackFrame::Init { ast });
                            }
                            for &ast in args {
                                stack.push(StackFrame::Init { ast });
                            }
                            return_ty.map(|ast| stack.push(StackFrame::Init { ast }));
                            stack.push(StackFrame::PopScope { old_scope });
                            for &ast in items {
                                stack.push(StackFrame::Init { ast });
                            }
                            for &ast in body {
                                stack.push(StackFrame::Init { ast });
                            }
                        }
                        _ => unreachable!(),
                    }
                    queue.push(insert_column::<Hir>(scope_ast.into(), ast));
                }
                function_call @ (Ast::NameAccess { .. } | Ast::MemberAccess { .. }) => {
                    let (name, args) = match function_call {
                        Ast::NameAccess { name, args, .. } => (name, args.clone()),
                        Ast::MemberAccess {
                            name, inner, args, ..
                        } => (name, [inner].into_iter().chain(args).copied().collect()),
                        _ => unreachable!(),
                    };
                    let scope_items = visible_items.get(db, scope.unwrap()).unwrap();
                    // TODO: This unwrap is an assertion that all names are in scope
                    let function = *scope_items.items.get(&name[..]).unwrap();
                    queue.push(insert_column(Hir::FunctionCall { function, args }, ast));
                    match function_call {
                        Ast::NameAccess { args, .. } => {
                            for &ast in args {
                                stack.push(StackFrame::Init { ast });
                            }
                        }
                        Ast::MemberAccess { inner, args, .. } => {
                            for &ast in args {
                                stack.push(StackFrame::Init { ast });
                            }
                            stack.push(StackFrame::Init { ast: *inner });
                        }
                        _ => unreachable!(),
                    }
                }
                node @ Ast::Argument { attributes, ty, .. } => {
                    for &ast in attributes {
                        stack.push(StackFrame::Init { ast });
                    }
                    stack.push(StackFrame::Init { ast: *ty });
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::Type { .. } => queue.push(insert_column::<Hir>(node.into(), ast)),
                node @ Ast::RecordConstructor { fields, fill, .. } => {
                    for &ast in fields {
                        stack.push(StackFrame::Init { ast });
                    }
                    for &ast in fill {
                        stack.push(StackFrame::Init { ast });
                    }
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::FieldInitializer { value, .. } => {
                    stack.push(StackFrame::Init { ast: *value });
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::StringLiteral { .. } => {
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::CharLiteral { .. } => {
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::ExternalFunction {
                    attributes, args, ..
                } => {
                    for &ast in attributes {
                        stack.push(StackFrame::Init { ast });
                    }
                    for &ast in args {
                        stack.push(StackFrame::Init { ast });
                    }
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                node @ Ast::AttributeList { attributes } => {
                    for &ast in attributes {
                        stack.push(StackFrame::Init { ast });
                    }
                    queue.push(insert_column::<Hir>(node.into(), ast))
                }
                Ast::Invalid { .. } => todo!(),
            },
            StackFrame::PopScope { old_scope } => *scope = old_scope,
        };
    }
}

pub fn attach_declarations(roots: &[Row]) {
    for &ast in roots {
        walk_declarations(ast);
    }
    for &ast in roots {
        build_visible_items(ast)
    }
}

pub fn walk_declarations(ast: Row) {
    let mut queue = DeferQueue::default();

    on_query(|mut query: Query<&Ast>| {
        let scope_names = &mut vec![];
        let scopes = &mut vec![];
        let mut stack = vec![StackFrame::Init { row: ast }];

        while let Some(frame) = stack.pop() {
            match frame {
                StackFrame::Init { row } => {
                    handle_stack_init(&mut query, row, scope_names, scopes, &mut stack, &mut queue)
                }
                StackFrame::Cleanup { row } => match query.get(row) {
                    Ast::Module { .. } | Ast::Function { .. } => {
                        scope_names.pop();
                    }
                    _ => unreachable!(
                        "internal compiler invariant violated; attempted to cleanup non-scope"
                    ),
                },
            }
        }
    });

    queue.finish();

    // Helpers

    enum StackFrame {
        Init { row: Row },
        Cleanup { row: Row },
    }

    fn handle_stack_init<'ast: 'stack, 'stack>(
        query: &mut Query<'ast, &Ast>,
        row: Row,
        scope_names: &mut Vec<&'stack str>,
        scopes: &mut Vec<Row>,
        stack: &mut Vec<StackFrame>,
        queue: &mut DeferQueue,
    ) {
        match query.get(row) {
            Ast::Module {
                name,
                items,
                ..
            } => {
                scope_names.push(name);
                scopes.push(row);
                queue.push(insert_column(Namespace::from_parts(scope_names), row));
                if let Some(scope) = scopes.last() {
                    queue.push(insert_column(ParentScope(*scope), row));
                }
                stack.push(StackFrame::Cleanup { row });
                for &row in items {
                    stack.push(StackFrame::Init { row })
                }
            }
            Ast::Function {
                name,
                items,
                ..
            } => {
                queue.push(insert_column(Item::Function, row));
                let namespace = Namespace::from_parts(scope_names);
                queue.push(insert_column(
                    QualifiedName {
                        namespace: namespace.0.clone(),
                        name: name.to_string(),
                    },
                    row,
                ));
                queue.push(insert_column(namespace, row));
                queue.push(insert_column(ParentScope(*scopes.last().unwrap()), row));
                scope_names.push(name);
                scopes.push(row);
                stack.push(StackFrame::Cleanup { row });
                for &row in items {
                    stack.push(StackFrame::Init { row })
                }
            }
            Ast::ExternalFunction {
                name, ..
            } => {
                queue.push(insert_column(Item::ExternalFunction, row));
                queue.push(insert_column(ParentScope(*scopes.last().unwrap()), row));
                queue.push(insert_column(
                    QualifiedName {
                        namespace: Namespace::from_parts(scope_names).0,
                        name: name.to_string(),
                    },
                    row,
                ))
            }
            kind => unreachable!(
                "internal compiler invariant violated; attempted to walk ast node of kind {:?} during declaration collection",
                kind.column_dbg::<Ast>()
            ),
        }
    }
}

pub fn build_visible_items(ast: Row) {
    let mut queue = DeferQueue::default();

    WORLD.with(|db| {
        let mut db = db.borrow_mut();
        let mut asts = db.query::<&Ast>();
        let mut parents = db.query::<&ParentScope>();
        let mut names = db.query::<&QualifiedName>();
        let mut stack = vec![ast];

        while let Some(scope) = stack.pop() {
            let mut vis = VisibleItems::default();

            match asts.get(&db, scope).unwrap() {
                scope_ast @ (Ast::Module { items, name, .. } | Ast::Function { items, name, .. }) => {
                    if let Ast::Function { .. } = scope_ast {
                        if let Ok(&ParentScope(parent)) = parents.get(&db, scope) {
                            match asts.get(&db, parent).unwrap() {
                                Ast::Module { items, .. } | Ast::Function { items, .. } => {
                                    for &item in items {
                                        let name = names.get(&db, item).unwrap().name.clone();
                                        vis.items.insert(name, item);
                                    }
                                },
                                _ => unreachable!("internal compiler invariant violated: parent scope was not a scope"),
                            }
                        }
                    }
                    vis.items.insert(name.to_string(), scope);
                    for &item in items {
                        let name = names.get(&db, item).unwrap().name.clone();
                        vis.items.insert(name, item);
                        stack.push(item);
                    }
                }
                Ast::ExternalFunction { .. } => {},
                _ => unreachable!("internal compiler invariant violated, attempted to build visible items for a non-scope node"),
            }

            queue.push(insert_column(vis, scope));
        }
    });

    queue.finish();
}

#[cfg(test)]
mod test {
    use apheleia_prism::ColumnDebug;

    use crate::Hir;

    #[test]
    fn cross_file_function_call() {
        let source1 = r#"
            module One

            fn Foo() {}
        "#;
        let tokens = apheleia_bookwyrm::lex(source1).unwrap();
        let root = apheleia_mandrake::parse(&tokens).unwrap();
        apheleia_hydra::lower_cst(source);
    }

    #[test]
    fn lower_putc() {
        let source = r#"
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
            "#;
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let root = apheleia_mandrake::parse(&tokens).unwrap();
        apheleia_hydra::lower_cst();
        crate::lower_ast(&[root]);
        println!("{:#?}", root.column_dbg::<Hir>());
    }
}
