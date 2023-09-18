use std::ops::Range;

use apheleia_bookwyrm::Token;
use apheleia_prism::prelude::*;
use apheleia_prism::EcsTreeDebug;
pub use parsing::*;

mod parsing;

#[derive(Debug)]
pub struct Backtrack;
pub type PResult<Value> = Result<Value, Backtrack>;

pub struct Context<'w, 's> {
    pub commands: Commands<'w, 's>,
    pub errors: (),
}

pub type Span = Range<usize>;
pub type Tokens<'src> = [(Token<'src>, Span)];

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

impl<Data: EcsTreeDebug> EcsTreeDebug for Punctuated<Data> {
    fn fmt<C>(&self, world: &World, f: &mut std::fmt::Formatter) -> std::fmt::Result
    where
        C: Component + EcsTreeDebug,
    {
        let mut list = f.debug_list();
        for (value, _) in &self.values {
            list.entry(&value.component_dbg::<C>(world));
            list.entry(&self.separator);
        }
        match &self.last {
            Some(value) => _ = list.entry(&value.component_dbg::<C>(world)),
            None => (),
        }
        list.finish()
    }
}

#[derive(Clone, Component, Debug, EcsTreeDebug)]
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
    pub fn spawn(self, ctx: &mut Context) -> Entity {
        ctx.commands.spawn(self).id()
    }

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

pub struct MandrakePlugin;

impl Plugin for MandrakePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Main,
            (parse_sources, apply_deferred).chain().in_set(ParseSources),
        );
    }
}

#[derive(Clone, Debug, Hash, SystemSet, PartialEq, Eq)]
pub struct ParseSources;

pub fn parse_sources(c: Commands, sources: Res<SourceFiles>) {
    let mut ctx = Context {
        commands: c,
        errors: (),
    };
    for (_, source) in sources.files.iter() {
        // TODO: Dump these errors into errors/turn into invalid nodes
        let tokens = apheleia_bookwyrm::lex(source).unwrap();
        let root = parse(&mut ctx, &tokens[..]).unwrap();
        ctx.commands.entity(root).insert(RootNode);
    }
}

pub fn parse(ctx: &mut Context, mut tokens: &Tokens) -> std::result::Result<Entity, ()> {
    (|ctx: &mut Context, input: &mut &Tokens| {
        let attributes = parse_attribute_lists(ctx, input)?;
        parse_module(attributes).parse(ctx, input)
    })
    .parse_whole(ctx, &mut tokens)
    .map_err(|_| ())
}

fn string(_: &mut Context, input: &mut &Tokens) -> PResult<(Span, String)> {
    input
        .next()
        .and_then(|(tok, span)| match tok {
            Token::String(string) => Some((span, string.to_string())),
            _ => None,
        })
        .ok_or(Backtrack)
}

fn char(_: &mut Context, input: &mut &Tokens) -> PResult<(Span, String)> {
    input
        .next()
        .and_then(|(tok, span)| match tok {
            Token::Char(char) => Some((span, char.to_string())),
            _ => None,
        })
        .ok_or(Backtrack)
}

pub fn punctuated<'src, P, Output>(
    mut parser: P,
    separator: Token<'src>,
) -> impl Parser<Output = Punctuated<Output>> + 'src
where
    P: MutParser<Output = Output> + 'src,
    for<'a> &'a mut P: MutParser<Output = Output>,
{
    move |ctx: &mut Context, input: &mut &Tokens| {
        let values = repeat(.., (&mut parser, just(separator))).parse(ctx, input)?;
        let last = opt(parser).parse(ctx, input)?;

        Ok(Punctuated {
            values,
            last,
            separator: format!("{separator:?}"),
        })
    }
}

pub fn parse_module_braced_contents(
    ctx: &mut Context,
    input: &mut &Tokens,
) -> PResult<((Span, Span), Vec<Entity>)> {
    let open_brace = just(Token::OpenBrace).parse(ctx, input)?;
    let items = parse_items(ctx, input)?;
    let close_brace = just(Token::CloseBrace).parse(ctx, input)?;
    Ok(((open_brace, close_brace), items))
}

pub fn parse_module(attributes: Vec<Entity>) -> impl Parser<Output = Entity> {
    move |ctx: &mut Context, input: &mut &Tokens| {
        let (module, name) = (just(Token::Module), ident).parse(ctx, input)?;
        let (braces, contents) = match opt(parse_module_braced_contents).parse(ctx, input)? {
            Some((braces, contents)) => (Some(braces), contents),
            _ => (None, parse_items(ctx, input)?),
        };
        Ok(Cst::Module {
            attributes,
            module,
            name,
            braces,
            items: contents,
        }
        .spawn(ctx))
    }
}

pub fn parse_item(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let attributes = parse_attribute_lists(ctx, input)?;

    match input.peek().ok_or(Backtrack)?.0 {
        Token::Fn => parse_function(attributes).parse(ctx, input),
        Token::External => parse_external_func(attributes).parse(ctx, input),
        Token::Module => parse_module(attributes).parse(ctx, input),
        _ => Err(Backtrack),
    }
}

pub fn parse_items(ctx: &mut Context, input: &mut &Tokens) -> PResult<Vec<Entity>> {
    repeat(.., parse_item).parse(ctx, input)
}

pub fn parse_function(attributes: Vec<Entity>) -> impl Parser<Output = Entity> {
    move |ctx: &mut Context, input: &mut &[(Token<'_>, Range<usize>)]| {
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
            .parse(ctx, input)?;
        Ok(Cst::Function {
            attributes,
            function,
            name,
            parens: (open_paren, close_paren),
            args,
            return_ty,
            braces: (open_brace, close_brace),
            body,
            items,
        }
        .spawn(ctx))
    }
}

pub fn parse_function_args(ctx: &mut Context, input: &mut &Tokens) -> PResult<Punctuated<Entity>> {
    punctuated(parse_function_arg, Token::Comma).parse(ctx, input)
}

pub fn parse_function_arg(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let (attributes, name, colon, ty) =
        (parse_attribute_lists, ident, just(Token::Colon), parse_type).parse(ctx, input)?;
    Ok(Cst::FunctionArg {
        attributes,
        name,
        colon,
        ty,
    }
    .spawn(ctx))
}

pub fn parse_field_init(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let (name, colon, value) = (ident, just(Token::Colon), parse_expr).parse(ctx, input)?;
    Ok(Cst::FieldInitializer { name, colon, value }.spawn(ctx))
}

/// Returns a tuple of (exprs, items)
pub fn parse_exprs_and_items(
    ctx: &mut Context,
    input: &mut &Tokens,
) -> PResult<(Vec<Entity>, Vec<Entity>)> {
    let mut exprs = vec![];
    let mut items = vec![];

    loop {
        let checkpoint = *input;
        match parse_item(ctx, input) {
            Ok(item) => {
                items.push(item);
                continue;
            }
            Err(Backtrack) => *input = checkpoint,
        }
        let checkpoint = *input;
        match parse_expr(ctx, input) {
            Ok(expr) => {
                exprs.push(expr);
                continue;
            }
            Err(Backtrack) => {
                *input = checkpoint;
                break;
            }
        }
    }

    Ok((exprs, items))
}

pub fn parse_expr_or_item(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    alt((parse_item, parse_expr)).parse(ctx, input)
}

pub fn parse_exprs(ctx: &mut Context, input: &mut &Tokens) -> PResult<Vec<Entity>> {
    repeat(.., parse_expr).parse(ctx, input)
}

pub fn parse_atomic_expr(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    alt((
        parse_record_ctor,
        parse_name_access,
        parse_string_literal,
        parse_char_literal,
    ))
    .parse(ctx, input)
}

pub fn parse_record_ctor(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let (name, open_brace) = (ident, just(Token::OpenBrace)).parse(ctx, input)?;
    let (fields, fill, close_brace) = (
        punctuated(parse_field_init, Token::Comma),
        opt((just(Token::DoubleDot), parse_expr)),
        just(Token::CloseBrace),
    )
        .parse(ctx, input)?;
    Ok(Cst::RecordConstructor {
        name,
        braces: (open_brace, close_brace),
        fields,
        fill,
    }
    .spawn(ctx))
}

pub fn parse_name_access(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let (name, args) = (
        ident,
        opt((
            just(Token::OpenParen),
            parse_expr_list,
            just(Token::CloseParen),
        )),
    )
        .parse(ctx, input)?;
    Ok(Cst::NameAccess { name, args }.spawn(ctx))
}

pub fn parse_string_literal(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let lit = string(ctx, input)?;
    Ok(Cst::StringLiteral { lit }.spawn(ctx))
}

pub fn parse_char_literal(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let lit = char(ctx, input)?;
    Ok(Cst::CharLiteral { lit }.spawn(ctx))
}

pub fn parse_expr(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    alt((parse_member_access, parse_atomic_expr)).parse(ctx, input)
}

pub fn parse_expr_list(ctx: &mut Context, input: &mut &Tokens) -> PResult<Punctuated<Entity>> {
    punctuated(parse_expr, Token::Comma).parse(ctx, input)
}

pub fn parse_member_access(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let mut inner = parse_atomic_expr(ctx, input)?;
    let mut extension = parse_member_access_extension(ctx, input)?;

    let mut checkpoint = *input;
    while let Ok(ext) = parse_member_access_extension(ctx, input) {
        let (dot, name, args) = extension;
        inner = Cst::MemberAccess {
            inner,
            dot,
            name,
            args,
        }
        .spawn(ctx);
        extension = ext;
        checkpoint = *input;
    }
    *input = checkpoint;

    let (dot, name, args) = extension;
    Ok(Cst::MemberAccess {
        inner,
        dot,
        name,
        args,
    }
    .spawn(ctx))
}

pub type ArgumentList = (Span, Punctuated<Entity>, Span);

pub fn parse_member_access_extension(
    ctx: &mut Context,
    input: &mut &Tokens,
) -> PResult<(Span, (Span, String), Option<ArgumentList>)> {
    (
        just(Token::Dot),
        ident,
        opt((
            just(Token::OpenParen),
            punctuated(parse_expr, Token::Comma),
            just(Token::CloseParen),
        )),
    )
        .parse(ctx, input)
}

pub fn parse_type(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let name = ident(ctx, input)?;
    Ok(Cst::Type { name }.spawn(ctx))
}

pub fn parse_type_list(ctx: &mut Context, input: &mut &Tokens) -> PResult<Punctuated<Entity>> {
    punctuated(parse_type, Token::Comma).parse(ctx, input)
}

pub fn parse_external_func(attributes: Vec<Entity>) -> impl Parser<Output = Entity> {
    |ctx: &mut Context, input: &mut &Tokens| {
        let (external, function, name, open_paren, args, close_paren) = (
            just(Token::External),
            just(Token::Fn),
            ident,
            just(Token::OpenParen),
            parse_type_list,
            just(Token::CloseParen),
        )
            .parse(ctx, input)?;
        Ok(Cst::ExternalFunc {
            external,
            attributes,
            function,
            name,
            parens: (open_paren, close_paren),
            args,
        }
        .spawn(ctx))
    }
}

pub fn parse_attribute_list(ctx: &mut Context, input: &mut &Tokens) -> PResult<Entity> {
    let (open_bracket, attributes, close_bracket) = (
        just(Token::OpenBracket),
        parse_expr_list,
        just(Token::CloseBracket),
    )
        .parse(ctx, input)?;
    Ok(Cst::AttributeList {
        brackets: (open_bracket, close_bracket),
        attributes,
    }
    .spawn(ctx))
}

pub fn parse_attribute_lists(ctx: &mut Context, input: &mut &Tokens) -> PResult<Vec<Entity>> {
    repeat(.., parse_attribute_list).parse(ctx, input)
}

#[cfg(test)]
mod test {
    use apheleia_bookwyrm::{lex, Token};
    use apheleia_prism::prelude::bevy_ecs::system::CommandQueue;
    use apheleia_prism::prelude::*;

    use crate::{
        just, opt, parse_attribute_list, parse_attribute_lists, parse_expr, parse_exprs,
        parse_function, parse_function_args, parse_item, parse_items, Context, Cst, MandrakePlugin,
        Parser, RootNode,
    };

    fn test_source_as<Output: EcsTreeDebug>(source: &str, parser: impl Parser<Output = Output>) {
        println!("Source: {source:?}");
        let tokens = lex(source).unwrap();
        let mut input = &tokens[..];
        let mut queue = CommandQueue::default();
        let mut world = World::new();
        let mut ctx = Context {
            commands: Commands::new(&mut queue, &world),
            errors: (),
        };
        let cst = parser.parse_whole(&mut ctx, &mut input).unwrap();
        queue.apply(&mut world);
        println!("{:#?}", cst.component_dbg::<Cst>(&world))
    }

    #[test]
    fn parse_char_into() {
        test_source_as("'W'.Into", parse_expr);
    }

    #[test]
    fn parse_ffi_default() {
        test_source_as("FFI.Default", parse_expr);
    }

    #[test]
    fn parse_ffi_default_fill() {
        test_source_as("..FFI.Default", opt((just(Token::DoubleDot), parse_expr)));
    }

    #[test]
    fn parse_putc_expr() {
        test_source_as("Putc('W'.Into)", parse_expr);
    }

    #[test]
    fn parse_putc_exprs() {
        test_source_as("'W'.Into", parse_exprs);
    }

    #[test]
    fn parse_empty_func_args() {
        test_source_as("", parse_function_args);
    }

    #[test]
    fn parse_putc_function() {
        test_source_as("fn Main() { Putc('W'.Into) }", parse_function(vec![]));
    }

    #[test]
    fn parse_putc_main_items() {
        test_source_as(
            r#"
            fn Main() {
                Putc('W'.Into)
            }
            "#,
            parse_items,
        );
    }

    #[test]
    fn parse_ffi_record() {
        test_source_as(
            r#"FFI { source: "C", name: "putc", ..FFI.Default }"#,
            parse_expr,
        );
    }

    #[test]
    fn parse_ffi_attribute_list() {
        test_source_as(
            r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#,
            parse_attribute_list,
        );
    }

    #[test]
    fn parse_ffi_attribute_lists() {
        test_source_as(
            r#"[FFI { source: "C", name: "putc", ..FFI.Default }]"#,
            parse_attribute_lists,
        );
    }

    #[test]
    fn parse_ffi_extern_putc() {
        test_source_as(
            r#"
            [FFI { source: "C", name: "putc", ..FFI.Default }]
            external fn Putc(U8)
            "#,
            parse_item,
        );
    }

    #[test]
    fn parse_putc() {
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
        let mut compiler = build_compiler(&[source]);
        compiler.add_plugins(MandrakePlugin).run_once();
        let cst = compiler
            .world
            .query_filtered::<Entity, With<RootNode>>()
            .single(&compiler.world);
        println!("{:#?}", cst.component_dbg::<Cst>(&compiler.world));
    }
}
