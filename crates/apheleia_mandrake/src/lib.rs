use std::ops::Range;

use apheleia_bookwyrm::Token;
use apheleia_prism::{create_row, derive_column, OnColumn, Row};
use winnow::combinator::{alt, cut_err, fail, opt, peek, repeat};
use winnow::error::{ErrMode, ErrorKind, ParseError, ParserError};
use winnow::stream::Stream;
use winnow::token::any;
use winnow::{dispatch, PResult, Parser};

pub type Span = Range<usize>;
pub type SpanToken = (Token, Span);
pub type SpanTokens<'i> = &'i [SpanToken];
pub type Tokens<'o, 'i> = &'o mut &'i [SpanToken];
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

derive_column!(Cst);
#[derive(Clone, Debug)]
pub enum Cst {
    Module {
        /// any attached attribute lists
        attributes: Vec<Row>,
        /// the `module` keyword
        module: Span,
        /// the identifier of the module
        name: Span,
        /// the `{` and `}` tokens, if present
        braces: Option<(Span, Span)>,
        /// the items of the module
        items: Vec<Row>,
    },
    Function {
        /// any attached attribute lists
        attributes: Vec<Row>,
        /// the `fn` keyword
        function: Span,
        /// the identifier of the function
        name: Span,
        /// the argument parens
        parens: (Span, Span),
        /// the arguments
        args: Punctuated<Row>,
        /// the `{` and `}` tokens
        braces: (Span, Span),
        /// the contents of the function
        body: Vec<Row>,
        /// any items declared in the body of the function
        items: Vec<Row>,
    },
    FunctionArg {
        /// the attributes of the argument
        attributes: Vec<Row>,
        /// the identifier of the argument
        name: Span,
        /// the `:` token
        colon: Span,
        /// the type of the argument
        ty: Row,
    },
    RecordConstructor {
        /// the identifier of the type to construct
        name: Span,
        /// the `{` and `}` tokens
        braces: (Span, Span),
        /// the field initializers
        fields: Punctuated<Row>,
        /// the `..expr` part of the declaration; declares that this constructor is actually overwriting
        /// the value given by the expression. The first span is the `..` token.
        fill: Option<(Span, Row)>,
    },
    FieldInitializer {
        /// the identifier of the field
        name: Span,
        /// the `:` token
        colon: Span,
        /// the value of the field
        value: Row,
    },
    NameAccess {
        /// the identifier of the element
        name: Span,
        /// the arguments, if any
        args: Option<(Span, Punctuated<Row>, Span)>,
    },
    /// This name is somewhat misleading. This can correspond to a field access, method
    /// call, or UFCS function call.
    MemberAccess {
        /// the expression having a member accessed
        inner: Row,
        /// the `.` token
        dot: Span,
        /// the identifier of the member
        name: Span,
        /// the arguments of a method/UFCS call, if any
        args: Option<(Span, Punctuated<Row>, Span)>,
    },
    StringLiteral {
        /// the literal
        lit: Span,
    },
    CharLiteral {
        /// the literal
        lit: Span,
    },
    Type {
        /// the identifier of the type
        name: Span,
    },
    External {
        /// any attached attribute lists
        attributes: Vec<Row>,
        /// the `external` identifier
        external: Span,
        /// the `{` and `}` tokens
        braces: (Span, Span),
        /// the items of this external declaration
        items: Vec<Row>,
    },
    ExternalFunc {
        /// any attached attribute lists
        attributes: Vec<Row>,
        /// the `fn` keyword
        function: Span,
        /// the identifier of the function
        name: Span,
        /// the argument parens
        parens: (Span, Span),
        /// the argument types
        args: Punctuated<Row>,
    },
    AttributeList {
        brackets: (Span, Span),
        attributes: Punctuated<Row>,
    },
    Invalid {
        span: Span,
    },
}

impl Cst {
    pub fn to_column(self) -> Row {
        create_row(self)
    }

    pub fn span(&self) -> Span {
        match self {
            Cst::Module {
                module,
                name,
                braces,
                items: contents,
                ..
            } => {
                module.start..match braces {
                    Some(braces) => braces.1.end,
                    None => match contents.last() {
                        Some(last) => last.on_column(Cst::span).end,
                        None => name.end,
                    },
                }
            }
            Cst::Function {
                function, braces, ..
            } => function.start..braces.1.end,
            Cst::FunctionArg { name, ty, .. } => name.start..ty.on_column(Cst::span).end,
            Cst::RecordConstructor { name, braces, .. } => name.start..braces.1.end,
            Cst::FieldInitializer { name, value, .. } => name.start..value.on_column(Cst::span).end,
            Cst::NameAccess { name, args } => {
                name.start..match args {
                    Some(args) => args.2.end,
                    None => name.end,
                }
            }
            Cst::MemberAccess {
                inner, name, args, ..
            } => {
                inner.on_column(Cst::span).start..match args {
                    Some(args) => args.2.end,
                    None => name.end,
                }
            }
            Cst::StringLiteral { lit } => lit.clone(),
            Cst::CharLiteral { lit } => lit.clone(),
            Cst::Type { name } => name.clone(),
            Cst::External {
                external, braces, ..
            } => external.start..braces.1.end,
            Cst::ExternalFunc {
                function, parens, ..
            } => function.start..parens.1.end,
            Cst::AttributeList { brackets, .. } => brackets.0.start..brackets.1.end,
            Cst::Invalid { span } => span.clone(),
        }
    }
}

pub fn parse(tokens: SpanTokens) -> std::result::Result<Row, ParseError<SpanTokens, ContextError>> {
    (|input: &mut &[(Token, Span)]| {
        let mut attributes = parse_attribute_lists(input)?;
        let mut parser = parse_module(&mut attributes);
        parser(input)
    })
    .parse(tokens)
}

pub fn just(token: Token) -> impl Fn(Tokens) -> Result<Span> {
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

pub fn punctuated<Data>(
    parser: impl Fn(Tokens) -> Result<Data>,
    separator: Token,
) -> impl Fn(Tokens) -> Result<Punctuated<Data>> {
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

pub fn invalid(stop_at: Token) -> impl Fn(Tokens) -> Result<Cst> {
    // let first =
    // Ok(Cst::Invalid {})
    |input| todo!()
}

pub fn parse_module_braced_contents(input: Tokens) -> Result<((Span, Span), Vec<Row>)> {
    let open_brace = just(Token::OpenBrace)(input)?;
    let items = parse_items(input)?;
    let close_brace = just(Token::CloseBrace)(input)?;
    Ok(((open_brace, close_brace), items))
}

pub fn parse_module(attributes: &mut Vec<Row>) -> impl FnMut(Tokens) -> Result<Row> + '_ {
    |input| {
        let (module, name) = (just(Token::Module), just(Token::Ident)).parse_next(input)?;
        let (braces, contents) = match parse_module_braced_contents(input) {
            Ok((braces, contents)) => (Some(braces), contents),
            _ => (None, parse_items(input)?),
        };
        Ok(Cst::Module {
            attributes: std::mem::take(attributes),
            module,
            name,
            braces,
            items: contents,
        }
        .to_column())
    }
}

pub fn parse_item(input: Tokens) -> Result<Row> {
    let mut attributes = parse_attribute_lists(input)?;
    dispatch!(peek(any);
        (Token::Fn, _) => parse_function(&mut attributes),
        (Token::External, _) => parse_external(&mut attributes),
        (Token::Module, _) => parse_module(&mut attributes),
        _ => fail,
    )
    .parse_next(input)
}

pub fn parse_items(input: Tokens) -> Result<Vec<Row>> {
    repeat(.., parse_item).parse_next(input)
}

pub fn parse_function(attributes: &mut Vec<Row>) -> impl FnMut(Tokens) -> Result<Row> + '_ {
    move |input| {
        let (function, name, open_paren, args, close_paren, open_brace, (body, items), close_brace) =
            (
                just(Token::Fn),
                just(Token::Ident),
                just(Token::OpenParen),
                parse_function_args,
                just(Token::CloseParen),
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
            braces: (open_brace, close_brace),
            body,
            items,
        }
        .to_column())
    }
}

pub fn parse_function_args(input: Tokens) -> Result<Punctuated<Row>> {
    punctuated(parse_function_arg, Token::Comma).parse_next(input)
}

pub fn parse_function_arg(input: Tokens) -> Result<Row> {
    let (attributes, name, colon, ty) = (
        parse_attribute_lists,
        just(Token::Ident),
        just(Token::Colon),
        parse_type,
    )
        .parse_next(input)?;
    Ok(Cst::FunctionArg {
        attributes,
        name,
        colon,
        ty,
    }
    .to_column())
}

pub fn parse_field_init(input: Tokens) -> Result<Row> {
    let (name, colon, value) =
        (just(Token::Ident), just(Token::Colon), parse_expr).parse_next(input)?;
    Ok(Cst::FieldInitializer { name, colon, value }.to_column())
}

/// Returns a tuple of (exprs, items)
pub fn parse_exprs_and_items(input: Tokens) -> Result<(Vec<Row>, Vec<Row>)> {
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

pub fn parse_expr_or_item(input: Tokens) -> Result<Row> {
    alt((parse_item, parse_expr, fail)).parse_next(input)
}

pub fn parse_exprs(input: Tokens) -> Result<Vec<Row>> {
    repeat(.., parse_expr).parse_next(input)
}

pub fn parse_atomic_expr(input: Tokens) -> Result<Row> {
    alt((
        parse_record_ctor,
        parse_name_access,
        parse_string_literal,
        parse_char_literal,
    ))
    .parse_next(input)
}

pub fn parse_record_ctor(input: Tokens) -> Result<Row> {
    let (name, open_brace) = (just(Token::Ident), just(Token::OpenBrace)).parse_next(input)?;
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

pub fn parse_name_access(input: Tokens) -> Result<Row> {
    let (name, args) = (
        just(Token::Ident),
        opt((
            just(Token::OpenParen),
            cut_err(parse_expr_list),
            cut_err(just(Token::CloseParen)),
        )),
    )
        .parse_next(input)?;
    Ok(Cst::NameAccess { name, args }.to_column())
}

pub fn parse_string_literal(input: Tokens) -> Result<Row> {
    let lit = just(Token::String)(input)?;
    Ok(Cst::StringLiteral { lit }.to_column())
}

pub fn parse_char_literal(input: Tokens) -> Result<Row> {
    let lit = just(Token::Char)(input)?;
    Ok(Cst::CharLiteral { lit }.to_column())
}

pub fn parse_expr(input: Tokens) -> Result<Row> {
    alt((parse_member_access, parse_atomic_expr)).parse_next(input)
}

pub fn parse_expr_list(input: Tokens) -> Result<Punctuated<Row>> {
    punctuated(parse_expr, Token::Comma).parse_next(input)
}

pub fn parse_member_access(input: Tokens) -> Result<Row> {
    let (inner, dot, name, args) = (
        parse_atomic_expr,
        just(Token::Dot),
        just(Token::Ident),
        opt((
            just(Token::OpenParen),
            parse_expr_list,
            just(Token::CloseParen),
        )),
    )
        .parse_next(input)?;
    Ok(Cst::MemberAccess {
        inner,
        dot,
        name,
        args,
    }
    .to_column())
}

pub fn parse_member_access_extension(
    input: Tokens,
) -> Result<(Span, Span, Option<(Span, Punctuated<Row>, Span)>)> {
}

pub fn parse_type(input: Tokens) -> Result<Row> {
    let name = just(Token::Ident)(input)?;
    Ok(Cst::Type { name }.to_column())
}

pub fn parse_type_list(input: Tokens) -> Result<Punctuated<Row>> {
    punctuated(parse_type, Token::Comma).parse_next(input)
}

pub fn parse_external(attributes: &mut Vec<Row>) -> impl FnMut(Tokens) -> Result<Row> + '_ {
    |input| {
        let (external, open_brace, contents, close_brace) = (
            just(Token::External),
            just(Token::OpenBrace),
            parse_external_items,
            just(Token::CloseBrace),
        )
            .parse_next(input)?;
        Ok(Cst::External {
            attributes: std::mem::take(attributes),
            external,
            braces: (open_brace, close_brace),
            items: contents,
        }
        .to_column())
    }
}

pub fn parse_external_items(input: Tokens) -> Result<Vec<Row>> {
    repeat(.., parse_external_item).parse_next(input)
}

pub fn parse_external_item(input: Tokens) -> Result<Row> {
    let attributes = parse_attribute_lists(input)?;
    dispatch!(peek(any);
        (Token::Fn, _) => parse_external_func(&attributes),
        _ => fail,
    )
    .parse_next(input)
}

pub fn parse_external_func(attributes: &[Row]) -> impl Fn(Tokens) -> Result<Row> + '_ {
    |input| {
        let (function, name, open_paren, args, close_paren) = (
            just(Token::Fn),
            just(Token::Ident),
            just(Token::OpenParen),
            parse_type_list,
            just(Token::CloseParen),
        )
            .parse_next(input)?;
        Ok(Cst::ExternalFunc {
            attributes: attributes.to_vec(),
            function,
            name,
            parens: (open_paren, close_paren),
            args,
        }
        .to_column())
    }
}

pub fn parse_attribute_list(input: Tokens) -> Result<Row> {
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

pub fn parse_attribute_lists(input: Tokens) -> Result<Vec<Row>> {
    repeat(.., parse_attribute_list).parse_next(input)
}

#[cfg(test)]
mod test {
    use apheleia_bookwyrm::lex;

    use crate::{
        parse_attribute_list, parse_attribute_lists, parse_expr, parse_exprs, parse_external,
        parse_function, parse_function_args, parse_items, parse_module,
    };

    #[test]
    pub fn parse_char_into() {
        let source = "'W'.Into";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let putc = parse_expr(&mut cursor).unwrap();
        println!("{putc:?}");
    }

    #[test]
    pub fn parse_putc_expr() {
        let source = "Putc('W'.Into)";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let putc = parse_expr(&mut cursor).unwrap();
        println!("{putc:?}");
    }

    #[test]
    pub fn parse_putc_exprs() {
        let source = "Putc('W'.Into)";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let putc = parse_exprs(&mut cursor).unwrap();
        println!("{putc:?}");
    }

    #[test]
    pub fn parse_empty_func_args() {
        let source = "";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let punct = parse_function_args(&mut cursor).unwrap();
        println!("{punct:?}");
    }

    #[test]
    pub fn parse_putc_function() {
        let source = "fn Main() { Putc('W'.Into) }";
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let func = parse_function(&mut vec![])(&mut cursor).unwrap();
        println!("{func:?}");
    }

    #[test]
    pub fn parse_putc_main_items() {
        let source = r#"
            fn Main() {
                Putc('W'.Into)
            }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let items = parse_items(&mut cursor).unwrap();
        println!("{items:?}");
    }

    #[test]
    pub fn parse_ffi_record() {
        let source = r#"FFI { source: "C", name: "putc", ..FFI.Default }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let parsed = parse_expr(&mut cursor).unwrap();
        println!("{parsed:?}");
    }

    #[test]
    pub fn parse_ffi_attribute() {
        let source = r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let parsed = parse_attribute_list(&mut cursor).unwrap();
        println!("{parsed:?}");
    }

    #[test]
    pub fn parse_ffi_attribute_list() {
        let source = r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let parsed = parse_attribute_lists(&mut cursor).unwrap();
        println!("{parsed:?}");
    }

    #[test]
    pub fn parse_ffi_extern_putc() {
        let source = r#"external {
            [FFI { source: "C", name: "putc", ..FFI.Default }]
            fn Putc(U8)
        }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let parsed = parse_external(&mut vec![])(&mut cursor).unwrap();
        println!("{parsed:?}");
    }

    #[test]
    pub fn parse_putc() {
        let source = r#"module Main

            external {
                [FFI { source: "C", name: "putc", ..FFI.Default }]
                fn Putc(U8)
            }

            fn Main() {
                Putc('W'.Into)
            }"#;
        let tokens = lex(source).unwrap();
        let mut cursor = &tokens[..];
        let module = parse_module(&mut vec![])(&mut cursor).unwrap();
        println!("{module:?}");
    }
}
