use std::ops::Range;

use apheleia_bookwyrm::Token;
use apheleia_prism::prelude::*;
use winnow::combinator::{alt, cut_err, fail, opt, peek, repeat};
use winnow::error::{ErrMode, ErrorKind, ParseError, ParserError};
use winnow::stream::Stream;
use winnow::token::any;
use winnow::{dispatch, PResult, Parser};

pub type Span = Range<usize>;
pub type SpanToken<'src> = (Token<'src>, Span);
pub type Tokens<'src> = [SpanToken<'src>];
pub type Result<Value> = PResult<Value, ContextError>;
pub type ContextError = winnow::error::ContextError<Context>;

#[derive(Debug, Clone)]
pub enum Context {}

#[derive(Clone)]
pub struct Punctuated<Data> {
    /// vec of data and punctuation
    pub values: Vec<(Data, Span)>,
    /// a final value
    pub last: Option<Data>,
    /// the string representation of the token separator
    pub separator: String,
}

impl<Data> Punctuated<Data> {
    pub fn to_vec(&self) -> Vec<Data>
    where
        Data: Clone,
    {
        self.values
            .iter()
            .map(|(data, _)| data)
            .chain(self.last.as_ref())
            .map(Data::clone)
            .collect()
    }
}

impl<Data: std::fmt::Debug> std::fmt::Debug for Punctuated<Data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for (value, _) in &self.values {
            list.entry(&value);
            list.entry(&self.separator);
        }
        match &self.last {
            Some(value) => _ = list.entry(&value),
            None => (),
        }
        list.finish()
    }
}

// impl<Data: ColumnDebug> ColumnDebug for Punctuated<Data> {
//     fn fmt<Col>(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
//     where
//         Col: Column + ColumnDebug,
//     {
//         let mut list = f.debug_list();
//         for (value, _) in &self.values {
//             list.entry(&value.column_dbg::<Col>());
//             list.entry(&self.separator);
//         }
//         match &self.last {
//             Some(value) => _ = list.entry(&value.column_dbg::<Col>()),
//             None => (),
//         }
//         list.finish()
//     }
// }

#[derive(Clone, Component, Debug)]
pub enum Cst {
    Module {
        /// any attached attribute lists
        attributes: Vec<Entity>,
        /// the `module` keyword
        module: Span,
        /// the identifier of the module
        name: (Span, String),
        /// the `{` and `}` tokens, if present
        braces: Option<(Span, Span)>,
        /// the items of the module
        items: Vec<Entity>,
    },
    Function {
        /// any attached attribute lists
        attributes: Vec<Entity>,
        /// the `fn` keyword
        function: Span,
        /// the identifier of the function
        name: (Span, String),
        /// the argument parens
        parens: (Span, Span),
        /// the arguments
        args: Punctuated<Entity>,
        /// the `->` token and return type, if any
        return_ty: Option<(Span, Entity)>,
        /// the `{` and `}` tokens
        braces: (Span, Span),
        /// the contents of the function
        body: Vec<Entity>,
        /// any items declared in the body of the function
        items: Vec<Entity>,
    },
    FunctionArg {
        /// the attributes of the argument
        attributes: Vec<Entity>,
        /// the identifier of the argument
        name: (Span, String),
        /// the `:` token
        colon: Span,
        /// the type of the argument
        ty: Entity,
    },
    RecordConstructor {
        /// the identifier of the type to construct
        name: (Span, String),
        /// the `{` and `}` tokens
        braces: (Span, Span),
        /// the field initializers
        fields: Punctuated<Entity>,
        /// the `..expr` part of the declaration; declares that this constructor is actually overwriting
        /// the value given by the expression. The first span is the `..` token.
        fill: Option<(Span, Entity)>,
    },
    FieldInitializer {
        /// the identifier of the field
        name: (Span, String),
        /// the `:` token
        colon: Span,
        /// the value of the field
        value: Entity,
    },
    NameAccess {
        /// the identifier of the element
        name: (Span, String),
        /// the arguments, if any
        args: Option<(Span, Punctuated<Entity>, Span)>,
    },
    /// This name is somewhat misleading. This can correspond to a field access, method
    /// call, or UFCS function call.
    MemberAccess {
        /// the expression having a member accessed
        inner: Entity,
        /// the `.` token
        dot: Span,
        /// the identifier of the member
        name: (Span, String),
        /// the arguments of a method/UFCS call, if any
        args: Option<(Span, Punctuated<Entity>, Span)>,
    },
    StringLiteral {
        /// the literal
        lit: (Span, String),
    },
    CharLiteral {
        /// the literal
        lit: (Span, String),
    },
    Type {
        /// the identifier of the type
        name: (Span, String),
    },
    ExternalFunc {
        /// any attached attribute lists
        attributes: Vec<Entity>,
        /// the `external` keyword
        external: Span,
        /// the `fn` keyword
        function: Span,
        /// the identifier of the function
        name: (Span, String),
        /// the argument parens
        parens: (Span, Span),
        /// the argument types
        args: Punctuated<Entity>,
    },
    AttributeList {
        brackets: (Span, Span),
        attributes: Punctuated<Entity>,
    },
    Invalid {
        span: Span,
    },
}

impl Cst {
    // pub fn span(&self) -> Span {
    //     match self {
    //         Cst::Module {
    //             module,
    //             name,
    //             braces,
    //             items: contents,
    //             ..
    //         } => {
    //             module.start..match braces {
    //                 Some(braces) => braces.1.end,
    //                 None => match contents.last() {
    //                     Some(last) => last.on_column(Cst::span).end,
    //                     None => name.0.end,
    //                 },
    //             }
    //         }
    //         Cst::Function {
    //             function, braces, ..
    //         } => function.start..braces.1.end,
    //         Cst::FunctionArg { name, ty, .. } => name.0.start..ty.on_column(Cst::span).end,
    //         Cst::RecordConstructor { name, braces, .. } => name.0.start..braces.1.end,
    //         Cst::FieldInitializer { name, value, .. } => {
    //             name.0.start..value.on_column(Cst::span).end
    //         }
    //         Cst::NameAccess { name, args } => {
    //             name.0.start..match args {
    //                 Some(args) => args.2.end,
    //                 None => name.0.end,
    //             }
    //         }
    //         Cst::MemberAccess {
    //             inner, name, args, ..
    //         } => {
    //             inner.on_column(Cst::span).start..match args {
    //                 Some(args) => args.2.end,
    //                 None => name.0.end,
    //             }
    //         }
    //         Cst::StringLiteral { lit } => lit.0.clone(),
    //         Cst::CharLiteral { lit } => lit.0.clone(),
    //         Cst::Type { name } => name.0.clone(),
    //         Cst::ExternalFunc {
    //             function, parens, ..
    //         } => function.start..parens.1.end,
    //         Cst::AttributeList { brackets, .. } => brackets.0.start..brackets.1.end,
    //         Cst::Invalid { span } => span.clone(),
    //     }
    // }
}

pub fn parse<'toks, 'src>(
    tokens: &'toks Tokens<'src>,
) -> std::result::Result<Entity, ParseError<&'toks Tokens<'src>, ContextError>> {
    (|input: &mut &Tokens| {
        let mut attributes = parse_attribute_lists(input)?;
        let mut parser = parse_module(&mut attributes);
        parser(input)
    })
    .parse(tokens)
}

pub fn just(token: Token) -> impl Fn(&mut &Tokens) -> Result<Span> + '_ {
    move |input| {
        let start = input.checkpoint();
        let value = input
            .next_token()
            .and_then(|(tok, span)| (tok == token).then_some(span))
            .ok_or_else(|| ErrMode::from_error_kind(input, ErrorKind::Token));
        if value.is_err() {
            input.reset(start);
        }
        value
    }
}

pub fn ident(input: &mut &Tokens) -> Result<(Span, String)> {
    let start = input.checkpoint();
    let value = input
        .next_token()
        .and_then(|(tok, span)| match tok {
            Token::Ident(ident) => Some((span, ident.to_string())),
            _ => None,
        })
        .ok_or_else(|| ErrMode::from_error_kind(input, ErrorKind::Token));
    if value.is_err() {
        input.reset(start);
    }
    value
}

pub fn string(input: &mut &Tokens) -> Result<(Span, String)> {
    let start = input.checkpoint();
    let value = input
        .next_token()
        .and_then(|(tok, span)| match tok {
            Token::String(string) => Some((span, string.to_string())),
            _ => None,
        })
        .ok_or_else(|| ErrMode::from_error_kind(input, ErrorKind::Token));
    if value.is_err() {
        input.reset(start);
    }
    value
}

pub fn char(input: &mut &Tokens) -> Result<(Span, String)> {
    let start = input.checkpoint();
    let value = input
        .next_token()
        .and_then(|(tok, span)| match tok {
            Token::Char(char) => Some((span, char.to_string())),
            _ => None,
        })
        .ok_or_else(|| ErrMode::from_error_kind(input, ErrorKind::Token));
    if value.is_err() {
        input.reset(start);
    }
    value
}

pub fn punctuated<'src, Data>(
    parser: impl Fn(&mut &Tokens<'src>) -> Result<Data> + 'src,
    separator: Token<'src>,
) -> impl Fn(&mut &Tokens<'src>) -> Result<Punctuated<Data>> + 'src {
    move |input| {
        let values = repeat(.., (&parser, just(separator))).parse_next(input)?;
        let check = input.checkpoint();
        let last = parser(input).ok();
        if last.is_none() {
            input.reset(check);
        }
        Ok(Punctuated {
            values,
            last,
            separator: format!("{separator:?}"),
        })
    }
}

pub fn invalid(stop_at: Token) -> impl Fn(&mut &Tokens) -> Result<Cst> {
    // let first =
    // Ok(Cst::Invalid {})
    |input| todo!()
}

pub fn parse_module_braced_contents(input: &mut &Tokens) -> Result<((Span, Span), Vec<Entity>)> {
    let open_brace = just(Token::OpenBrace)(input)?;
    let items = parse_items(input)?;
    let close_brace = just(Token::CloseBrace)(input)?;
    Ok(((open_brace, close_brace), items))
}

pub fn parse_module<'a>(
    mut c: Commands<'a, 'a>,
    attributes: &'a mut Vec<Entity>,
) -> impl FnMut(&mut &Tokens) -> Result<Entity> + 'a {
    move |input| {
        let (module, name) = (just(Token::Module), ident).parse_next(input)?;
        let (braces, contents) = match parse_module_braced_contents(input) {
            Ok((braces, contents)) => (Some(braces), contents),
            _ => (None, parse_items(input)?),
        };
        Ok(c.spawn(Cst::Module {
            attributes: std::mem::take(attributes),
            module,
            name,
            braces,
            items: contents,
        })
        .id())
    }
}

pub fn parse_item(input: &mut &Tokens) -> Result<Entity> {
    let mut attributes = parse_attribute_lists(input)?;
    dispatch!(peek(any);
        (Token::Fn, _) => parse_function(&mut attributes),
        (Token::External, _) => parse_external_func(&mut attributes),
        (Token::Module, _) => parse_module(&mut attributes),
        _ => fail,
    )
    .parse_next(input)
}

pub fn parse_items(input: &mut &Tokens) -> Result<Vec<Entity>> {
    repeat(.., parse_item).parse_next(input)
}

pub fn parse_function(
    attributes: &mut Vec<Entity>,
) -> impl FnMut(&mut &Tokens) -> Result<Entity> + '_ {
    move |input| {
        let (
            function,
            name,
            open_paren,
            args,
            close_paren,
            return_ty,
            open_brace,
            (body, items),
            close_brace,
        ) = (
            just(Token::Fn),
            ident,
            just(Token::OpenParen),
            parse_function_args,
            just(Token::CloseParen),
            opt((just(Token::RArrow), parse_type)),
            just(Token::OpenBrace),
            parse_exprs_and_items,
            just(Token::CloseBrace),
        )
            .parse_next(input)?;
        Ok(Cst::Function {
            attributes: std::mem::take(attributes),
            function,
            name,
            parens: (open_paren, close_paren),
            args,
            return_ty,
            braces: (open_brace, close_brace),
            body,
            items,
        }
        .to_column())
    }
}

pub fn parse_function_args(input: &mut &Tokens) -> Result<Punctuated<Entity>> {
    punctuated(parse_function_arg, Token::Comma).parse_next(input)
}

pub fn parse_function_arg(input: &mut &Tokens) -> Result<Entity> {
    let (attributes, name, colon, ty) =
        (parse_attribute_lists, ident, just(Token::Colon), parse_type).parse_next(input)?;
    Ok(Cst::FunctionArg {
        attributes,
        name,
        colon,
        ty,
    }
    .to_column())
}

pub fn parse_field_init(input: &mut &Tokens) -> Result<Entity> {
    let (name, colon, value) = (ident, just(Token::Colon), parse_expr).parse_next(input)?;
    Ok(Cst::FieldInitializer { name, colon, value }.to_column())
}

/// Returns a tuple of (exprs, items)
pub fn parse_exprs_and_items(input: &mut &Tokens) -> Result<(Vec<Entity>, Vec<Entity>)> {
    let mut exprs = vec![];
    let mut items = vec![];

    loop {
        match parse_item(input) {
            Ok(item) => {
                items.push(item);
                continue;
            }
            Err(ErrMode::Backtrack(_)) => (),
            Err(err) => return Err(err),
        }
        match parse_expr(input) {
            Ok(expr) => {
                exprs.push(expr);
                continue;
            }
            Err(ErrMode::Backtrack(_)) => break,
            Err(err) => return Err(err),
        }
    }

    Ok((exprs, items))
}

pub fn parse_expr_or_item(input: &mut &Tokens) -> Result<Entity> {
    alt((parse_item, parse_expr, fail)).parse_next(input)
}

pub fn parse_exprs(input: &mut &Tokens) -> Result<Vec<Entity>> {
    repeat(.., parse_expr).parse_next(input)
}

pub fn parse_atomic_expr(input: &mut &Tokens) -> Result<Entity> {
    alt((
        parse_record_ctor,
        parse_name_access,
        parse_string_literal,
        parse_char_literal,
    ))
    .parse_next(input)
}

pub fn parse_record_ctor(input: &mut &Tokens) -> Result<Entity> {
    let (name, open_brace) = (ident, just(Token::OpenBrace)).parse_next(input)?;
    let (fields, fill, close_brace) = cut_err((
        punctuated(parse_field_init, Token::Comma),
        opt((just(Token::DoubleDot), parse_expr)),
        just(Token::CloseBrace),
    ))
    .parse_next(input)?;
    Ok(Cst::RecordConstructor {
        name,
        braces: (open_brace, close_brace),
        fields,
        fill,
    }
    .to_column())
}

pub fn parse_name_access(input: &mut &Tokens) -> Result<Entity> {
    let (name, args) = (
        ident,
        opt((
            just(Token::OpenParen),
            cut_err(parse_expr_list),
            cut_err(just(Token::CloseParen)),
        )),
    )
        .parse_next(input)?;
    Ok(Cst::NameAccess { name, args }.to_column())
}

pub fn parse_string_literal(input: &mut &Tokens) -> Result<Entity> {
    let lit = string(input)?;
    Ok(Cst::StringLiteral { lit }.to_column())
}

pub fn parse_char_literal(input: &mut &Tokens) -> Result<Entity> {
    let lit = char(input)?;
    Ok(Cst::CharLiteral { lit }.to_column())
}

pub fn parse_expr(input: &mut &Tokens) -> Result<Entity> {
    alt((parse_member_access, parse_atomic_expr)).parse_next(input)
}

pub fn parse_expr_list(input: &mut &Tokens) -> Result<Punctuated<Entity>> {
    punctuated(parse_expr, Token::Comma).parse_next(input)
}

pub fn parse_member_access(input: &mut &Tokens) -> Result<Entity> {
    let mut inner = parse_atomic_expr(input)?;
    let mut extension = parse_member_access_extension(input)?;

    loop {
        match parse_member_access_extension(input) {
            Ok(ext) => {
                let (dot, name, args) = extension;
                inner = Cst::MemberAccess {
                    inner,
                    dot,
                    name,
                    args,
                }
                .to_column();
                extension = ext;
            }
            Err(ErrMode::Backtrack(_)) => break,
            Err(err) => return Err(err),
        }
    }

    let (dot, name, args) = extension;
    Ok(Cst::MemberAccess {
        inner,
        dot,
        name,
        args,
    }
    .to_column())
}

pub type ArgumentList = (Span, Punctuated<Entity>, Span);

pub fn parse_member_access_extension(
    input: &mut &Tokens,
) -> Result<(Span, (Span, String), Option<ArgumentList>)> {
    (
        just(Token::Dot),
        ident,
        opt((
            just(Token::OpenParen),
            punctuated(parse_expr, Token::Comma),
            just(Token::CloseParen),
        )),
    )
        .parse_next(input)
}

pub fn parse_type(input: &mut &Tokens) -> Result<Entity> {
    let name = ident(input)?;
    Ok(Cst::Type { name }.to_column())
}

pub fn parse_type_list(input: &mut &Tokens) -> Result<Punctuated<Entity>> {
    punctuated(parse_type, Token::Comma).parse_next(input)
}

pub fn parse_external_func(
    attributes: &mut Vec<Entity>,
) -> impl FnMut(&mut &Tokens) -> Result<Entity> + '_ {
    |input| {
        let (external, function, name, open_paren, args, close_paren) = (
            just(Token::External),
            just(Token::Fn),
            ident,
            just(Token::OpenParen),
            parse_type_list,
            just(Token::CloseParen),
        )
            .parse_next(input)?;
        Ok(Cst::ExternalFunc {
            external,
            attributes: std::mem::take(attributes),
            function,
            name,
            parens: (open_paren, close_paren),
            args,
        }
        .to_column())
    }
}

pub fn parse_attribute_list(input: &mut &Tokens) -> Result<Entity> {
    let (open_bracket, attributes, close_bracket) = (
        just(Token::OpenBracket),
        parse_expr_list,
        just(Token::CloseBracket),
    )
        .parse_next(input)?;
    Ok(Cst::AttributeList {
        brackets: (open_bracket, close_bracket),
        attributes,
    }
    .to_column())
}

pub fn parse_attribute_lists(input: &mut &Tokens) -> Result<Vec<Entity>> {
    repeat(.., parse_attribute_list).parse_next(input)
}

#[cfg(test)]
mod test {
    use apheleia_bookwyrm::lex;
    use apheleia_prism::ColumnDebug;

    use crate::{
        parse_attribute_list, parse_attribute_lists, parse_expr, parse_exprs, parse_external_func,
        parse_function, parse_function_args, parse_items, parse_module, Cst,
    };

    #[test]
    pub fn parse_char_into() {
        let source = "'W'.Into";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_expr(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_putc_expr() {
        let source = "Putc('W'.Into)";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_expr(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_putc_exprs() {
        let source = "Putc('W'.Into)";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_exprs(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_empty_func_args() {
        let source = "";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_function_args(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_putc_function() {
        let source = "fn Main() { Putc('W'.Into) }";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_function(&mut vec![])(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_putc_main_items() {
        let source = r#"
            fn Main() {
                Putc('W'.Into)
            }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_items(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_ffi_record() {
        let source = r#"FFI { source: "C", name: "putc", ..FFI.Default }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_expr(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_ffi_attribute() {
        let source = r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_attribute_list(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_ffi_attribute_list() {
        let source = r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_attribute_lists(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_ffi_extern_putc() {
        let source = r#"
            [FFI { source: "C", name: "putc", ..FFI.Default }]
            external fn Putc(U8)
        "#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let mut attrs = parse_attribute_lists(&mut cursor).unwrap();
        let root = parse_external_func(&mut attrs)(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }

    #[test]
    pub fn parse_putc() {
        let source = r#"
            module Main

            [FFI { source: "C", name: "putc", ..FFI.Default }]
            external fn Putc(U8)

            fn Main() {
                Putc('W'.TryInto.Unwrap)
            }
        "#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let root = parse_module(&mut vec![])(&mut cursor).unwrap();
        println!("{:#?}", root.column_dbg::<Cst>());
    }
}
