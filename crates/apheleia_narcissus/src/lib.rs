use std::collections::HashMap;

use apheleia_hydra::Ast;
use apheleia_prism::{
    derive_column, insert_column, on_query, ApplyQueue, DeferQueue, Query, Row, WORLD,
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
#[derive(Default)]
pub struct VisibleItems {
    pub items: HashMap<String, Row>,
}

derive_column!(Hir);
pub enum Hir {
    Module {
        attributes: Vec<Row>,
        name: String,
        items: Vec<Row>,
    },
    Function {
        attributes: Vec<Row>,
        name: String,
        args: Vec<Row>,
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
    FunctionCall {
        function: Row,
        args: Vec<Row>,
    },
    StringLiteral {
        string: String,
    },
    CharLiteral {
        char: char,
    },
    External {
        attributes: Vec<Row>,
        items: Vec<Row>,
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
        let mut stack = vec![StackFrame::Init { row: ast }];

        while let Some(frame) = stack.pop() {
            match frame {
                StackFrame::Init { row } => {
                    handle_stack_init(&mut query, row, scope_names, &mut stack, &mut queue)
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
                queue.push(insert_column(Namespace::from_parts(scope_names), row));
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
                scope_names.push(name);
                stack.push(StackFrame::Cleanup { row });
                for &row in items {
                    stack.push(StackFrame::Init { row })
                }
            }
            Ast::External { items, .. } => {
                for &row in items {
                    stack.push(StackFrame::Init { row })
                }
            }
            Ast::ExternalFunction {
                name, ..
            } => {
                queue.push(insert_column(Item::ExternalFunction, row));
                queue.push(insert_column(
                    QualifiedName {
                        namespace: Namespace::from_parts(scope_names).0,
                        name: name.to_string(),
                    },
                    row,
                ))
            }
            kind => unreachable!(
                "internal compiler invariant violated; attempted to walk ast node of kind {kind:?} during declaration collection"
            ),
        }
    }
}

pub fn build_visible_items(ast: Row) {
    let mut queue = DeferQueue::default();

    WORLD.with(|world| {
        let mut world = world.borrow_mut();
        let mut asts = world.query::<&Ast>();
        let mut names = world.query::<&QualifiedName>();
        let mut stack = vec![ast];

        while let Some(scope) = stack.pop() {
            let mut vis = VisibleItems::default();

            match asts.get(&world, scope).unwrap() {
                Ast::Module { items, .. } | Ast::Function { items, .. } => {
                    for &item in items {
                        let name = names.get(&world, item).unwrap().name.clone();
                        vis.items.insert(name, item);
                        stack.push(item);
                    }
                }
                _ => unreachable!("internal compiler invariant violated, attempted to build visible items for a non-scope node"),
            }

            queue.push(insert_column(vis, scope));
        }
    });

    queue.finish();
}
