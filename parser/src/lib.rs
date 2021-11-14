use chumsky::error::*;
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
enum Instr {
    CombS,
    CombK,
    CombD,
    CombI,
    Map,
    Comp,
    Eq,
    Add,
    Var(u32),
    Lambda(Vec<Self>),
}

fn parser() -> impl Parser<char, Vec<Instr>, Error = Simple<char>> {
    let spaces = just(' ').repeated();
    let int = text::int::<char, Simple<char>>(10).map(|s| s.parse().unwrap());
    recursive(|bf| {
        spaces.ignore_then(
            bf.delimited_by('{', '}')
                .map(Instr::Lambda)
                .or(just('K').to(Instr::CombK))
                .or(just('S').to(Instr::CombS))
                .or(just('I').to(Instr::CombI))
                .or(just('D').to(Instr::CombD))
                .or(just('$').to(Instr::Map))
                .or(just('+').to(Instr::Add))
                .or(just('=').to(Instr::Eq))
                .or(just('w')
                    .ignore_then(int)
                    .then_ignore(spaces)
                    .map(Instr::Var))
                .then_ignore(spaces)
                .repeated(),
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_ski() {
        let (res, _) = parser().parse_recovery("S K I");
        assert_eq!(res.unwrap(), vec![Instr::CombS, Instr::CombK, Instr::CombI]);
    }

    #[test]
    fn test_lambda() {
        let (res, _) = parser().parse_recovery("{D w1 I}");
        assert_eq!(
            res.unwrap(),
            vec![Instr::Lambda(vec![
                Instr::CombD,
                Instr::Var(1),
                Instr::CombI
            ])]
        );
    }

    #[test]
    fn test_nested_lambda() {
        let (res, _) = parser().parse_recovery("{D {+ w1 w2} I}");
        assert_eq!(
            res.unwrap(),
            vec![Instr::Lambda(vec![
                Instr::CombD,
                Instr::Lambda(vec![Instr::Add, Instr::Var(1), Instr::Var(2)]),
                Instr::CombI
            ])]
        );
    }
}
