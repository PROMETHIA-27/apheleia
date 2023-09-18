use std::ops::RangeBounds;

use apheleia_bookwyrm::Token;
use impl_trait_for_tuples::impl_for_tuples;

use crate::{Backtrack, Context, PResult, Span, Tokens};

pub trait Stream {
    fn next(&mut self) -> Option<(Token, Span)>;

    fn peek(&self) -> Option<(Token, Span)>;
}

impl Stream for &Tokens<'_> {
    fn next(&mut self) -> Option<(Token, Span)> {
        let tok = self.first();
        if tok.is_some() {
            *self = &self[1..];
        }
        tok.cloned()
    }

    fn peek(&self) -> Option<(Token, Span)> {
        self.first().cloned()
    }
}

pub trait Parser {
    type Output;

    fn parse(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output>;

    fn parse_whole(self, ctx: &mut Context, input: &mut &Tokens) -> Result<Self::Output, ()>
    where
        Self: Sized,
    {
        let value = self.parse(ctx, input);
        assert!(
            input.is_empty(),
            "parser did not consume entire input, remaining input: {input:?}"
        );
        value.map_err(|_| ())
    }

    fn inspect<F>(self, f: F) -> Inspect<Self, F>
    where
        Self: Sized,
        F: FnOnce((&Context, &Tokens, &PResult<Self::Output>)),
    {
        Inspect(self, f)
    }
}

pub struct Inspect<P, F>(P, F);

impl<P: Parser, F: FnOnce((&Context, &Tokens, &PResult<P::Output>))> Parser for Inspect<P, F> {
    type Output = P::Output;

    fn parse(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output> {
        let output = self.0.parse(ctx, input);
        (self.1)((ctx, input, &output));
        output
    }
}

impl<F, Output> Parser for F
where
    F: FnOnce(&mut Context, &mut &Tokens) -> PResult<Output>,
{
    type Output = Output;

    fn parse(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output> {
        self(ctx, input)
    }
}

pub trait MutParser: Parser {
    fn parse_mut(&mut self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output>;
}

impl<F, Output> MutParser for F
where
    F: FnMut(&mut Context, &mut &Tokens) -> PResult<Output>,
{
    fn parse_mut(&mut self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output> {
        self(ctx, input)
    }
}

#[impl_for_tuples(1, 16)]
impl Parser for Tuple {
    for_tuples!(
        type Output = ( #(Tuple::Output),* );
    );

    fn parse(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output> {
        Ok((for_tuples!(
            #(Tuple.parse(ctx, input)?),*
        )))
    }
}

#[impl_for_tuples(1, 16)]
impl MutParser for Tuple {
    fn parse_mut(&mut self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Self::Output> {
        Ok((for_tuples!(
            #(Tuple.parse_mut(ctx, input)?),*
        )))
    }
}

pub trait ParserAlts<Output> {
    fn parse_alts(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Output>;
}

#[impl_for_tuples(1, 16)]
#[tuple_types_custom_trait_bound(Parser)]
impl<Output> ParserAlts<Output> for Tuple {
    for_tuples!(where #(Tuple: Parser<Output = Output>)*);

    #[allow(clippy::almost_swapped)]
    fn parse_alts(self, ctx: &mut Context, input: &mut &Tokens) -> PResult<Output> {
        let start = *input;
        for_tuples!(#(
            *input = start;
            if let Ok(value) = Tuple.parse(ctx, input) {
                return Ok(value)
            }
        )*);
        Err(Backtrack)
    }
}

pub fn just(token: Token) -> impl MutParser<Output = Span> + '_ {
    move |_: &mut Context, input: &mut &Tokens| {
        let (tok, span) = input.next().ok_or(Backtrack)?;
        (tok == token).then_some(span).ok_or(Backtrack)
    }
}

pub fn ident(_: &mut Context, input: &mut &Tokens) -> PResult<(Span, String)> {
    input
        .next()
        .and_then(|(tok, span)| match tok {
            Token::Ident(ident) => Some((span, ident.to_string())),
            _ => None,
        })
        .ok_or(Backtrack)
}

pub fn alt<Output>(parsers: impl ParserAlts<Output>) -> impl Parser<Output = Output> {
    |ctx: &mut Context, input: &mut &Tokens| parsers.parse_alts(ctx, input)
}

pub fn opt<Output>(parser: impl Parser<Output = Output>) -> impl Parser<Output = Option<Output>> {
    |ctx: &mut Context, input: &mut &Tokens| {
        let start = *input;

        let result = parser.parse(ctx, input);

        if result.is_err() {
            *input = start;
        }

        Ok(result.ok())
    }
}

pub fn repeat<Output, Collection: FromIterator<Output>>(
    bounds: impl RangeBounds<usize>,
    mut parser: impl MutParser<Output = Output>,
) -> impl MutParser<Output = Collection> {
    move |ctx: &mut Context, input: &mut &Tokens| {
        let start = match bounds.start_bound() {
            std::ops::Bound::Included(n) => *n,
            std::ops::Bound::Excluded(n) => n + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match bounds.end_bound() {
            std::ops::Bound::Included(n) => n + 1,
            std::ops::Bound::Excluded(n) => *n,
            std::ops::Bound::Unbounded => usize::MAX,
        };

        let mut counter = 0;
        let mut checkpoint = *input;
        let result = std::iter::repeat_with(|| {
            checkpoint = *input;
            let res = parser.parse_mut(ctx, input);
            if res.is_err() {
                *input = checkpoint;
            }
            res
        })
        .take_while(|value| {
            if value.is_ok() && counter + 1 < end {
                counter += 1;
                true
            } else {
                false
            }
        })
        .map(Result::unwrap)
        .collect::<Collection>();

        if counter < start {
            Err(Backtrack)
        } else {
            Ok(result)
        }
    }
}
