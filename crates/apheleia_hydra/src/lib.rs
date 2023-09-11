use apheleia_mandrake::{Cst, Span};
use apheleia_prism::{
    derive_column, insert_column_soft, insert_to_row, on_query, ApplyQueue, ColumnDebug,
    DeferQueue, Query, Row, Without,
};

derive_column!(Ast);
#[derive(ColumnDebug)]
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
}

pub fn lower_cst() {
    let mut deferred = DeferQueue::default();
    on_query(|mut query: Query<(Row, &Cst), Without<Ast>>| {
        for (row, cst) in query.iter() {
            let ast = lower_cst_node(cst);

            deferred.push(insert_column(ast, row));
        }
    });
    deferred.finish();
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
    use apheleia_prism::ColumnDebug;

    use crate::{lower_cst, Ast};

    #[test]
    pub fn lower_triple_member_access() {
        let source = "foo.bar.baz.yeet";
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = apheleia_mandrake::parse_expr(&mut cursor).unwrap();
        lower_cst();
        println!("{:#?}", root.column_dbg::<Ast>());
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
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let root = apheleia_mandrake::parse(&tokens).unwrap();
        lower_cst();
        println!("{:#?}", root.column_dbg::<Ast>());
    }
}
