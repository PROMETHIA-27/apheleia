use std::fmt::Debug;

use apheleia_mandrake::{Cst, Span};
use apheleia_prism::{
    derive_column, insert_column, insert_to_row, on_query, ApplyQueue, DeferQueue, OnColumn, Query,
    Row, WORLD,
};

derive_column!(Ast);
pub enum Ast {
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
    NameAccess {
        name: String,
        args: Vec<Row>,
    },
    MemberAccess {
        inner: Row,
        name: String,
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
    Invalid {
        span: Span,
    },
}

impl Ast {
    pub fn add_to(self, row: Row) -> Row {
        insert_to_row(self, row)
    }

    /// Traverse the AST in preorder
    pub fn walk(&self, f: &mut impl FnMut(&Ast)) {
        f(self);
        let mut walk_ast = |ast: &'_ Ast| Ast::walk(ast, f);
        match &self {
            Ast::Module {
                attributes, items, ..
            } => {
                for ast in attributes.iter().chain(items) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::Function {
                attributes,
                args,
                body,
                ..
            } => {
                for ast in attributes.iter().chain(args).chain(body) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::Argument { attributes, ty, .. } => {
                for ast in attributes.iter().chain([ty]) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::Type { .. } => (),
            Ast::RecordConstructor { fields, fill, .. } => {
                for ast in fields.iter().chain(fill) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::FieldInitializer { value, .. } => value.on_column(walk_ast),
            Ast::NameAccess { args, .. } => {
                for ast in args {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::MemberAccess { inner, args, .. } => {
                for ast in args.iter().chain([inner]) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::StringLiteral { .. } => (),
            Ast::CharLiteral { .. } => (),
            Ast::External { attributes, items } => {
                for ast in attributes.iter().chain(items) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::ExternalFunction {
                attributes, args, ..
            } => {
                for ast in attributes.iter().chain(args) {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::AttributeList { attributes } => {
                for ast in attributes {
                    ast.on_column(&mut walk_ast);
                }
            }
            Ast::Invalid { .. } => (),
        }
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugAstRow<'row>(&'row Row);

        impl<'row> Debug for DebugAstRow<'row> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                WORLD.with(|world| {
                    let world = world.borrow();
                    let ast = world.get::<Ast>(*self.0).unwrap();
                    ast.fmt(f)
                })
            }
        }

        fn to_debug(vec: &[Row]) -> Vec<DebugAstRow> {
            vec.iter().map(DebugAstRow).collect()
        }

        match self {
            Self::Module {
                attributes,
                name,
                items,
            } => f
                .debug_struct("Module")
                .field("attributes", &to_debug(attributes))
                .field("name", name)
                .field("items", &to_debug(items))
                .finish(),
            Self::Function {
                attributes,
                name,
                args,
                body,
                items,
            } => f
                .debug_struct("Function")
                .field("attributes", &to_debug(attributes))
                .field("name", name)
                .field("args", &to_debug(args))
                .field("body", &to_debug(body))
                .field("items", &to_debug(items))
                .finish(),
            Self::Argument {
                attributes,
                name,
                ty,
            } => f
                .debug_struct("Argument")
                .field("attributes", &to_debug(attributes))
                .field("name", name)
                .field("ty", &DebugAstRow(ty))
                .finish(),
            Self::Type { name } => f.debug_struct("Type").field("name", name).finish(),
            Self::RecordConstructor { name, fields, fill } => f
                .debug_struct("RecordConstructor")
                .field("name", name)
                .field("fields", &to_debug(fields))
                .field("fill", &fill.as_ref().map(DebugAstRow))
                .finish(),
            Self::FieldInitializer { name, value } => f
                .debug_struct("FieldInitializer")
                .field("name", name)
                .field("value", &DebugAstRow(value))
                .finish(),
            Self::NameAccess { name, args } => f
                .debug_struct("NameAccess")
                .field("name", name)
                .field("args", &to_debug(args))
                .finish(),
            Self::MemberAccess { inner, name, args } => f
                .debug_struct("MemberAccess")
                .field("inner", &DebugAstRow(inner))
                .field("name", name)
                .field("args", &to_debug(args))
                .finish(),
            Self::StringLiteral { string } => f
                .debug_struct("StringLiteral")
                .field("string", string)
                .finish(),
            Self::CharLiteral { char } => {
                f.debug_struct("CharLiteral").field("char", char).finish()
            }
            Self::External { attributes, items } => f
                .debug_struct("External")
                .field("attributes", &to_debug(attributes))
                .field("items", &to_debug(items))
                .finish(),
            Self::ExternalFunction {
                attributes,
                name,
                args,
            } => f
                .debug_struct("ExternalFunction")
                .field("attributes", &to_debug(attributes))
                .field("name", name)
                .field("args", &to_debug(args))
                .finish(),
            Self::AttributeList { attributes } => f
                .debug_struct("AttributeList")
                .field("attributes", &to_debug(attributes))
                .finish(),
            Self::Invalid { span } => f.debug_struct("Invalid").field("span", span).finish(),
        }
    }
}

pub fn lower(source: &str) {
    let mut deferred = DeferQueue::default();
    on_query(|mut query: Query<(Row, &Cst)>| {
        for (row, cst) in query.iter() {
            let ast = lower_cst(source, cst);

            deferred.push(insert_column(ast, row));
        }
    });
    deferred.finish();
}

pub fn lower_cst(source: &str, cst: &Cst) -> Ast {
    match cst {
        Cst::Module {
            attributes,
            name,
            items,
            ..
        } => Ast::Module {
            attributes: attributes.clone(),
            name: source[name.clone()].to_string(),
            items: items.clone(),
        },
        Cst::Function {
            attributes,
            name,
            args,
            body,
            items,
            ..
        } => Ast::Function {
            attributes: attributes.clone(),
            name: source[name.clone()].to_string(),
            args: args.to_vec(),
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
            name: source[name.clone()].to_string(),
            ty: *ty,
        },
        Cst::RecordConstructor {
            name, fields, fill, ..
        } => Ast::RecordConstructor {
            name: source[name.clone()].to_string(),
            fields: fields.to_vec(),
            fill: fill.as_ref().map(|(_, fill)| *fill),
        },
        Cst::FieldInitializer { name, value, .. } => Ast::FieldInitializer {
            name: source[name.clone()].to_string(),
            value: *value,
        },
        Cst::NameAccess { name, args } => Ast::NameAccess {
            name: source[name.clone()].to_string(),
            args: args
                .as_ref()
                .map(|(_, args, _)| args.to_vec())
                .unwrap_or_default(),
        },
        Cst::MemberAccess {
            inner, name, args, ..
        } => Ast::MemberAccess {
            inner: *inner,
            name: source[name.clone()].to_string(),
            args: args
                .as_ref()
                .map(|(_, args, _)| args.to_vec())
                .unwrap_or_default(),
        },
        Cst::StringLiteral { lit } => Ast::StringLiteral {
            string: source[lit.start + 1..lit.end - 1].to_string(),
        },
        Cst::CharLiteral { lit } => Ast::CharLiteral {
            char: source[lit.start + 1..lit.end - 1].chars().next().unwrap(),
        },
        Cst::Type { name } => Ast::Type {
            name: source[name.clone()].to_string(),
        },
        Cst::External {
            attributes, items, ..
        } => Ast::External {
            attributes: attributes.clone(),
            items: items.clone(),
        },
        Cst::ExternalFunc {
            attributes,
            name,
            args,
            ..
        } => Ast::ExternalFunction {
            attributes: attributes.clone(),
            name: source[name.clone()].to_string(),
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
    use apheleia_prism::OnColumn;

    use crate::{lower, Ast};

    #[test]
    pub fn lower_triple_member_access() {
        let source = "foo.bar.baz.yeet";
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let mut cursor = &tokens[..];
        let parsed = apheleia_mandrake::parse_expr(&mut cursor).unwrap();
        lower(source);
        parsed.on_column(|ast: &Ast| {
            println!("{ast:#?}");
        })
    }

    #[test]
    pub fn lower_putc() {
        let source = r#"
            module Main

            external {
                [FFI { source: "C", name: "putc", ..FFI.Default }]
                fn Putc(U8)
            }

            fn Main() {
                Putc('W'.Into)
            }
            "#;
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let module = apheleia_mandrake::parse(&tokens).unwrap();
        lower(source);
        module.on_column(|ast: &Ast| {
            println!("{ast:#?}");
        });
    }
}
