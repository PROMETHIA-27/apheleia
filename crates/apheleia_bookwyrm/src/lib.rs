use logos::*;

#[derive(Debug)]
#[non_exhaustive]
pub enum LexerError {
    SomethingWentWrong,
}

pub fn lex(source: &str) -> Result<Vec<(Token, Span)>, LexerError> {
    Token::lexer(source)
        .spanned()
        .try_fold(vec![], |mut vec, (tok, span)| match tok {
            Ok(tok) => {
                vec.push((tok, span));
                Ok(vec)
            }
            Err(()) => Err(LexerError::SomethingWentWrong),
        })
}

#[derive(Clone, Copy, Debug, Logos, PartialEq, Eq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'source> {
    #[regex("[_a-zA-Z0-9]+", |str| str.slice())]
    Ident(&'source str),
    #[regex(r#""[^"]+""#, |str| str.slice())]
    String(&'source str),
    /// Invariant: The lexer must only produce char tokens that are surrounded by `'`
    /// and have exactly one character
    #[regex("'.'", |str| str.slice())]
    Char(&'source str),
    #[token("module")]
    Module,
    #[token("fn")]
    Fn,
    #[token("external")]
    External,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token(".")]
    Dot,
    #[token("..")]
    DoubleDot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("->")]
    RArrow,
}

#[cfg(test)]
mod test {
    use logos::Logos;

    use crate::Token::{self, *};

    #[test]
    fn lex_ident() {
        let mut lexer = Token::lexer("Main");
        let tok = lexer.next().unwrap();
        let tok = tok.unwrap();
        assert_eq!(tok, Ident("Main"));
    }

    #[test]
    fn lex_empty_module() {
        let lexer = Token::lexer("module Main");
        let toks: Result<Vec<_>, _> = lexer.collect();
        let toks = toks.unwrap();
        assert_eq!(&toks, &[Module, Ident("Main")]);
    }

    #[test]
    fn lex_string() {
        let mut lexer = Token::lexer(r#""text text2""#);
        let tok = lexer.next().unwrap();
        let tok = tok.unwrap();
        assert_eq!(tok, String("\"text text2\""));
    }

    #[test]
    fn lex_char() {
        let mut lexer = Token::lexer("'T'");
        let tok = lexer.next().unwrap();
        let tok = tok.unwrap();
        assert_eq!(tok, Char("'T'"));
    }

    #[test]
    fn lex_putc_w() {
        let lexer = Token::lexer(
            r#"module Main

            external {
                [FFI { source: "C", name: "putc", ..FFI.default }]
                fn Putc(U8);
            }

            fn Main() {
                Putc('W'.Into)
            }"#,
        );

        let toks: Result<Vec<_>, _> = lexer.collect();
        let toks = toks.unwrap();

        assert_eq!(
            &toks,
            &[
                Module,
                Ident("Main"),
                External,
                OpenBrace,
                OpenBracket,
                Ident("FFI"),
                OpenBrace,
                Ident("source"),
                Colon,
                String("\"C\""),
                Comma,
                Ident("name"),
                Colon,
                String("\"putc\""),
                Comma,
                DoubleDot,
                Ident("FFI"),
                Dot,
                Ident("default"),
                CloseBrace,
                CloseBracket,
                Fn,
                Ident("Putc"),
                OpenParen,
                Ident("U8"),
                CloseParen,
                Semicolon,
                CloseBrace,
                Fn,
                Ident("Main"),
                OpenParen,
                CloseParen,
                OpenBrace,
                Ident("Putc"),
                OpenParen,
                Char("'W'"),
                Dot,
                Ident("Into"),
                CloseParen,
                CloseBrace
            ]
        )
    }
}
