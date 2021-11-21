use chumsky::prelude::*;
use std::fmt;

/// Type of reserved symbols used in ULP
#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    True,
    False,
    Num(u32),
    Table(Vec<Literal>),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Num(n) => write!(f, "{}", n),
            Literal::Table(v) => {
                write!(f, "{{")?;
                if v.len() != 0 {
                    write!(f, "{}", v[0])?;
                    for i in &v[1..] {
                        write!(f, " {}", i)?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

/// Type of reserved symbols used in ULP
#[derive(Clone, PartialEq, Debug)]
pub enum Sym {
    /// S combinator
    CombS,
    /// K combinator
    CombK,
    /// D combinator
    CombD,
    /// I combinator
    CombI,
    /// Map operator ($)
    Map,
    /// Equality (=)
    Eq,
    /// Addition (+)
    Add,
    /// Local variables (w)
    Var(u32),
    /// Lambda abstractions
    Lambda(Vec<Self>),
    /// Literals
    Lit(Literal),
}

impl fmt::Display for Sym {
    /// Pretty printing for symbols
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sym::CombS => write!(f, "S"),
            Sym::CombK => write!(f, "K"),
            Sym::CombD => write!(f, "D"),
            Sym::CombI => write!(f, "I"),
            Sym::Map => write!(f, "$"),
            Sym::Eq => write!(f, "="),
            Sym::Add => write!(f, "+"),
            Sym::Var(i) => write!(f, "w{}", i),
            Sym::Lambda(instr) => {
                write!(f, "{{")?;
                if instr.len() != 0 {
                    write!(f, "{}", instr[0])?;
                    for i in &instr[1..] {
                        write!(f, " {}", i)?;
                    }
                }
                write!(f, "}}")
            }
            Sym::Lit(lit) => write!(f, "{}", lit),
        }
    }
}

/// Parser
pub fn parser() -> impl Parser<char, Vec<Sym>, Error = Simple<char>> {
    let spaces = just(' ').repeated();
    let int = text::int::<char, Simple<char>>(10).map(|s| s.parse().unwrap());
    recursive(|prog| {
        spaces.ignore_then(
            prog.delimited_by('{', '}')
                .map(Sym::Lambda)
                .or(just('K').to(Sym::CombK))
                .or(just('S').to(Sym::CombS))
                .or(just('I').to(Sym::CombI))
                .or(just('D').to(Sym::CombD))
                .or(just('$').to(Sym::Map))
                .or(just('+').to(Sym::Add))
                .or(just('=').to(Sym::Eq))
                .or(just('w').ignore_then(int).then_ignore(spaces).map(Sym::Var))
                .then_ignore(spaces)
                .repeated()
                .at_least(1),
        )
    })
}

// #$[1, 2, 3]
//
//           $
//          / \
//         #   [1, 2, 3]

#[derive(Clone, Debug)]
pub enum ComputationTree {
    Node {
        tag: Sym,
        left: Box<Self>,
        right: Box<Self>,
    },
    Leaf(Sym),
}

pub fn linear_check(prog: &[Sym]) -> Result<ComputationTree, &'static str> {
    fn step(prog: &[Sym], acc: ComputationTree) -> Result<ComputationTree, &'static str> {
        if let Some(sym) = prog.last() {
            match sym {
                Sym::Map | Sym::Eq | Sym::Add => {
                    if let Some(arg) = prog[..prog.len() - 1].last() {
                        step(
                            &prog[..prog.len() - 2],
                            ComputationTree::Node {
                                tag: sym.clone(),
                                left: Box::new(ComputationTree::Leaf(arg.clone())),
                                right: Box::new(acc),
                            },
                        )
                    } else {
                        Err("ill-structured : expected symbol after binary op")
                    }
                }
                _ => Err("ill-structured linear expression"),
            }
        } else {
            Ok(acc)
        }
    }
    prog.last().ok_or("No symbols !").and_then(|sym| match sym {
        Sym::Lit(_) | Sym::Var(_) => step(
            &prog[..(prog.len() - 1)],
            ComputationTree::Leaf(sym.clone()),
        ),
        _ => Err("ill-structured linear expression : expected variable or literal"),
    })
}

pub fn lambda_check(prog: Vec<Sym>) -> ComputationTree {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use Sym::*;
    #[test]
    fn test_ski() {
        let (res, _) = parser().parse_recovery("S K I");
        assert_eq!(res.unwrap(), vec![CombS, CombK, CombI]);
    }

    #[test]
    fn test_lambda() {
        let (res, _) = parser().parse_recovery("{D w1 I}");
        assert_eq!(res.unwrap(), vec![Sym::Lambda(vec![CombD, Var(1), CombI])]);
    }
    #[test]
    fn test_nested_lambda() {
        let (res, err) = parser().parse_recovery("{D {+ w1 w2} I}");
        if res.is_none() {
            println!("{:?}", err);
            assert!(false)
        }
        let r = res.unwrap();
        println!("{:?}", r);
        assert_eq!(
            r,
            vec![Lambda(vec![
                CombD,
                Lambda(vec![Add, Var(1), Var(2)]),
                CombI
            ])]
        );
    }

    #[test]
    fn test_linear_check1() {
        let e = vec![
            Lit(Literal::Num(1)),
            Add,
            Lit(Literal::Table(vec![
                Literal::Num(1),
                Literal::Num(2),
                Literal::Num(3),
            ])),
        ];
        println!("{:?}", linear_check(&e))
    }

    #[test]
    fn test_linear_check2() {
        let e = vec![
            Lit(Literal::Num(2)),
            Eq,
            Lit(Literal::Num(1)),
            Add,
            Lit(Literal::Table(vec![
                Literal::Num(1),
                Literal::Num(2),
                Literal::Num(3),
            ])),
        ];
        println!("{:?}", linear_check(&e))
    }

    #[test]
    fn test_linear_check_fail1() {
        let e = vec![
            Add,
            Lit(Literal::Table(vec![
                Literal::Num(1),
                Literal::Num(2),
                Literal::Num(3),
            ])),
        ];
        println!("{:?}", linear_check(&e))
    }
}
