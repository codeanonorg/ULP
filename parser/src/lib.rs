mod report;
mod spanned;
mod token;

use crate::token::Token;
use chumsky::{prelude::*, Stream};
use report::Report;
use report::{report_of_char_error, report_of_token_error};
use token::lexer;

pub use spanned::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Num(String),
    List(Vec<Spanned<Self>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sym {
    CombS,
    CombK,
    CombD,
    CombI,
    Map,
    Iota,
    Len,
    Reduce,
    Filter,
    Neg,
    And,
    Or,
    Eq,
    Add,
    Literal(Lit),
    Var(u32),
    Lambda(Vec<Spanned<Self>>),
}

impl Sym {
    pub fn lambda<I: IntoIterator<Item = Option<Spanned<Self>>>>(inner: I) -> Option<Self> {
        Some(Self::Lambda(inner.into_iter().collect::<Option<Vec<_>>>()?))
    }
}

fn literal() -> impl Parser<Token, Spanned<Lit>, Error = Simple<Token>> {
    use token::Dir::*;
    use Token::*;
    let int = filter_map(|span, tok| match tok {
        Num(n) => Ok(Lit::Num(n).spanned(span)),
        t => Err(Simple::expected_input_found(span, vec![], Some(t))),
    });
    recursive(|lit| {
        lit.repeated()
            .at_least(1)
            .delimited_by(Bracket(L), Bracket(R))
            .map_with_span(|l, span| Lit::List(l).spanned(span))
            .or(int)
    })
}

fn parser() -> impl Parser<Token, Option<Vec<Spanned<Sym>>>, Error = Simple<Token>> {
    use token::Dir::*;
    use Token::*;
    let var = filter_map(|span, tok| match tok {
        Var(i) => Ok(Sym::Var(i)),
        t => Err(Simple::expected_input_found(span, vec![], Some(t))),
    });
    let lit = literal().map(|l| Sym::Literal(l.value));
    recursive(|instr| {
        instr
            .delimited_by(Brace(L), Brace(R))
            .map_with_span(|v, span| Sym::lambda(v).map(|l| l.spanned(span)))
            .or(just(Ident("K".to_string()))
                .to(Sym::CombK)
                .or(just(Ident("S".to_string())).to(Sym::CombS))
                .or(just(Ident("I".to_string())).to(Sym::CombI))
                .or(just(Ident("D".to_string())).to(Sym::CombD))
                .or(just(Op("i".to_string())).to(Sym::Iota))
                .or(just(Op("$".to_string())).to(Sym::Map))
                .or(just(Op("+".to_string())).to(Sym::Add))
                .or(just(Op("#".to_string())).to(Sym::Len))
                .or(just(Op("=".to_string())).to(Sym::Eq))
                .or(just(Op("/".to_string())).to(Sym::Reduce))
                .or(just(Op("&".to_string())).to(Sym::And))
                .or(just(Op("|".to_string())).to(Sym::Or))
                .or(just(Op("!".to_string())).to(Sym::Neg))
                .or(just(Op("\\".to_string())).to(Sym::Filter))
                .or(lit)
                .or(var)
                .map_with_span(|s, span| Some(s.spanned(span))))
            .recover_with(nested_delimiters(Brace(L), Brace(R), [], |_| None))
            .repeated()
    })
    .map(|v| v.into_iter().collect::<Option<Vec<_>>>())
}

pub fn parse(src_id: impl Into<String>, input: &str) -> (Option<Vec<Spanned<Sym>>>, Vec<Report>) {
    let src_id = src_id.into();
    let slen = input.len();
    let (tokens, tokerr) = lexer().then_ignore(end()).parse_recovery(input);
    let tokerr = tokerr.into_iter().map({
        let src_id = src_id.clone();
        move |err| report_of_char_error(src_id.clone(), err)
    });
    if let Some(tokens) = tokens {
        let (instrs, err) = parser()
            .then_ignore(end())
            .parse_recovery(Stream::from_iter(
                slen..slen + 1,
                tokens.into_iter().map(Into::into),
            ));
        let tokerr = tokerr
            .chain(
                err.into_iter()
                    .map(move |err| report_of_token_error(src_id.clone(), err)),
            )
            .collect();
        if let Some(Some(instrs)) = instrs {
            (Some(instrs), tokerr)
        } else {
            (None, tokerr)
        }
    } else {
        (None, tokerr.collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Sym::*;
    macro_rules! assert_parse {
        ($input:expr, [$($e:expr),*]) => {{
            let input = $input;
            let (res, err) = parse("<test>", input);
            insta::assert_display_snapshot!(err
                .into_iter()
                .map(|r| {
                    use ariadne::Source;
                    let mut s = ::std::io::Cursor::new(Vec::new());
                    r.write(("<test>".into(), Source::from(input)), &mut s)
                        .unwrap();
                    String::from_utf8_lossy(&s.into_inner()).to_string()
                })
                .collect::<Vec<_>>()
                .join("\n"));
            insta::assert_debug_snapshot!(res);
        }};
    }
    #[test]
    fn test_ski() {
        assert_parse!("S K I", [CombS, CombK, CombI]);
    }

    #[test]
    fn test_lambda() {
        assert_parse!("{D w1 I}", [Lambda(vec![CombD, Var(1), CombI])])
    }

    #[test]
    fn test_nested_lambda() {
        assert_parse!(
            "{D {+ w1 w2} I}",
            [Lambda(vec![
                CombD,
                Lambda(vec![Add, Var(1), Var(2)]),
                CombI
            ])]
        );
    }
}
