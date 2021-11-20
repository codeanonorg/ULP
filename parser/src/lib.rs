mod report;
mod spanned;
mod token;

use crate::token::Token;
use chumsky::{prelude::*, Stream};
use report::Report;
use report::{report_of_char_error, report_of_token_error};
use token::lexer;

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    CombS,
    CombK,
    CombD,
    CombI,
    Map,
    Comp,
    Eq,
    Add,
    Num(String),
    Var(u32),
    Lambda(Vec<Self>),
}

impl Instr {
    pub fn lambda<I: IntoIterator<Item = Option<Self>>>(inner: I) -> Option<Self> {
        Some(Self::Lambda(inner.into_iter().collect::<Option<Vec<_>>>()?))
    }
}

fn parser() -> impl Parser<Token, Option<Vec<Instr>>, Error = Simple<Token>> {
    use token::Dir::*;
    use Token::*;
    let int = filter_map(|span, tok| match tok {
        Num(n) => Ok(Instr::Num(n)),
        t => Err(Simple::expected_input_found(span, vec![], Some(t))),
    });
    let var = filter_map(|span, tok| match tok {
        Var(i) => Ok(Instr::Var(i)),
        t => Err(Simple::expected_input_found(span, vec![], Some(t))),
    });
    recursive(|instr| {
        instr
            .delimited_by(Bracket(L), Bracket(R))
            .map(|v| Instr::lambda(v))
            .or(just(Ident("K".to_string()))
                .to(Instr::CombK)
                .or(just(Ident("S".to_string())).to(Instr::CombS))
                .or(just(Ident("I".to_string())).to(Instr::CombI))
                .or(just(Ident("D".to_string())).to(Instr::CombD))
                .or(just(Op("$".to_string())).to(Instr::Map))
                .or(just(Op("+".to_string())).to(Instr::Add))
                .or(just(Op("=".to_string())).to(Instr::Eq))
                .or(int)
                .or(var)
                .map(Some))
            .recover_with(nested_delimiters(Bracket(L), Bracket(R), [], |_| None))
            .repeated()
    })
    .map(|v| v.into_iter().collect::<Option<Vec<_>>>())
}

pub fn parse(src_id: impl Into<String>, input: &str) -> (Option<Vec<Instr>>, Vec<Report>) {
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
    use Instr::*;
    macro_rules! assert_parse {
        ($input:expr, [$($e:expr),*]) => {
            {
                use ariadne::Source;
                let input = $input;
                let (res, err) = parse("<test>", input);
                for report in err {
                    report.eprint(("<test>".into(), Source::from(input))).unwrap();
                }
                assert_eq!(res, Some(vec![$($e),*]));
            }
        };
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
