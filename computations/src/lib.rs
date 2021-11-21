use std::borrow::Borrow;
#[allow(dead_code)]
use std::fmt;

mod checker;
mod trees;

use parser::Sym;
use trees::{ComputationTree, Literal};

use crate::trees::{BinOp, Combinator};

fn count_variables(prog: &Vec<Sym>) -> u32 {
    prog.iter().fold(0, |acc, s| match s {
        Sym::Var(_) => acc + 1,
        _ => acc,
    })
}

fn to_tree<'a>(s: &Sym) -> Result<ComputationTree, &'static str> {
    match s {
        Sym::CombS => Ok(ComputationTree::CombOp(Combinator::S)),
        Sym::CombK => Ok(ComputationTree::CombOp(Combinator::K)),
        Sym::CombD => Ok(ComputationTree::CombOp(Combinator::D)),
        Sym::CombI => Ok(ComputationTree::CombOp(Combinator::I)),
        Sym::Map => Ok(ComputationTree::BinOpSym(BinOp::Map)),
        Sym::Comp => unreachable!(),
        Sym::Eq => Ok(ComputationTree::BinOpSym(BinOp::Eq)),
        Sym::Add => Ok(ComputationTree::BinOpSym(BinOp::Add)),
        Sym::Num(n) => Ok(ComputationTree::Lit(Literal::Num(n.clone()))),
        Sym::Var(v) => Ok(ComputationTree::Lit(Literal::Var(v.clone()))),
        Sym::Lambda(prog) => non_linear_check(prog).and_then(|body| {
            Ok(ComputationTree::Lambda {
                vars: count_variables(prog),
                body: Box::new(body),
            })
        }),
    }
}

struct State {
    consumed: usize,
    ignore: bool,
    accumulator: ComputationTree,
}

impl State {
    fn new(acc: ComputationTree) -> Self {
        State {
            consumed: 1,
            ignore: false,
            accumulator: acc,
        }
    }

    fn ignore(self) -> Self {
        State {
            ignore: false,
            ..self
        }
    }

    fn accumulate(self, op: BinOp, lhs: ComputationTree) -> Self {
        State {
            consumed: self.consumed + 1,
            ignore: true,
            accumulator: ComputationTree::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(self.accumulator),
            },
        }
    }

    fn is_done(&self, prog: &[Sym]) -> bool {
        self.consumed + 1 == prog.len()
    }
}

fn linear_check<'a>(prog: &[Sym]) -> Result<ComputationTree, &'static str> {
    let next = &prog[1..];
    let iter = &mut next.windows(2);
    let acc = match &prog[0] {
        Sym::Num(n) => Ok(ComputationTree::Lit(Literal::Num(n.clone()))),
        Sym::Var(v) => Ok(ComputationTree::Lit(Literal::Var(v.clone()))),
        _ => Err("Literal expected"),
    }?;
    iter.try_fold(State::new(acc), |state, pair| {
        if state.ignore {
            Ok(state.ignore())
        } else {
            println!("({:?}, {:?})", pair[0], pair[1]);
            match pair[0] {
                Sym::CombS | Sym::CombK | Sym::CombD | Sym::CombI => Err("Unexpected combinator"),
                Sym::Map => {
                    to_tree(&pair[1]).and_then(|tree| Ok(state.accumulate(BinOp::Map, tree)))
                }
                Sym::Add => {
                    to_tree(&pair[1]).and_then(|tree| Ok(state.accumulate(BinOp::Add, tree)))
                }
                Sym::Eq => to_tree(&pair[1]).and_then(|tree| Ok(state.accumulate(BinOp::Eq, tree))),
                Sym::Comp => todo!(),
                Sym::Num(_) => Err("expected operator, found 'Num'"),
                Sym::Var(_) => Err("expected operator, found 'Var'"),
                Sym::Lambda(_) => Err("expected operator, found 'Lambda'"),
            }
        }
    })
    .and_then(|state| {
        if state.is_done(prog) {
            Ok(state.accumulator)
        } else {
            println!("consumed : {}", state.consumed);
            Err("elements remaining")
        }
    })
}

fn non_linear_check<'a>(prog: &[Sym]) -> Result<ComputationTree, &'static str> {
    todo!()
}

pub fn check<'a>(prog: &Vec<Sym>) -> Result<ComputationTree, &'static str> {
    if prog.len() == 0 {
        Err("No symbols")
    } else {
        let prog = &mut prog.clone();
        prog.reverse();
        println!("debug : {:?}", prog);
        linear_check(&prog.clone())
    }
}

#[cfg(test)]
mod test {
    use parser::Sym;

    use crate::check;

    #[test]
    pub fn test_linear_check() {
        let prog = vec![
            // Sym::Num("1".to_string()),
            Sym::Map,
            Sym::Add,
            Sym::Map,
            Sym::Num("2".to_string()),
        ];
        match check(&prog) {
            Ok(res) => println!("Success: {:?}", res),
            Err(err) => println!("Error: {:?}", err),
        };
    }
}
