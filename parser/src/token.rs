use core::fmt;
use std::ops::Range;

use chumsky::{
    error::{Error, Simple},
    prelude::*,
    text::{ident, int, TextParser},
    Parser,
};
pub use Dir::*;
pub use Token::*;

use crate::spanned::{Spanned, SpannedExt};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir {
    L,
    R,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Op(String),
    Ident(String),
    Num(String),
    Var(u32),
    Paren(Dir),
    Bracket(Dir),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op(s) | Ident(s) | Num(s) => write!(f, "{}", s),
            Var(i) => write!(f, "w{}", i),
            Paren(L) => write!(f, "("),
            Paren(R) => write!(f, ")"),
            Bracket(L) => write!(f, "{{"),
            Bracket(R) => write!(f, "}}"),
        }
    }
}

pub(crate) fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let op = just('+')
        .or(just('='))
        .or(just('$'))
        .or(just('i'))
        .or(just('#'))
        .or(just('!'))
        .map(|c| c.to_string())
        .map(Op);
    let ident = ident().map(Ident);
    let num = int(10).map(Num);
    let parens = just('(').to(Paren(L)).or(just(')').to(Paren(R)));
    let brackets = just('{').to(Bracket(L)).or(just('}').to(Bracket(R)));
    let var = just('w')
        .ignore_then(int(10))
        .try_map(|s, Range { start, end }| {
            s.parse().map(Var).map_err(|_| {
                Simple::custom(start - 1..end, format!("Could not parse variable index"))
                    .with_label("Variables are of the form w# where # is a natural number")
            })
        });
    op.or(num)
        .or(var)
        .or(ident)
        .or(parens)
        .or(brackets)
        .padded()
        .map_with_span(|tok, span| tok.spanned(span))
        .recover_with(nested_delimiters('(', ')', [('[', ']'), ('{', '}')], |span| Paren(R).spanned(span)))
        .recover_with(nested_delimiters('[', ']', [('(', ')'), ('{', '}')], |span| Bracket(R).spanned(span)))
        .recover_with(nested_delimiters('{', '}', [('(', ')'), ('[', ']')], |span| Paren(R).spanned(span)))
        .repeated()
}
