#[allow(dead_code)]
mod trees;

use parser::Sym;
use trees::{ComputationTree, Literal, UnOp};

use crate::trees::{BinOp, Combinator};

// States for our state machine
// WaitForOp -> We are waiting for an operator as the next symbol
// WaitForVal(op) -> We are waiting for at least one symbol
//  This symbol will be the second parameter for the binary operator "op"
#[derive(Debug)]
enum State {
    WaitForOp,
    WaitForVal(BinOp),
}

// A datastructure to represent the current
// Computation tree together with the current state
#[derive(Debug)]
struct Accumulator {
    state: State,
    acc: ComputationTree,
}

impl Accumulator {
    // Build a new accumulator containing only the symbol "s" viewed
    // as a computation tree
    fn new(s: &Sym) -> Result<Self, &'static str> {
        Self::to_tree(s).and_then(|acc| {
            Ok(Accumulator {
                state: State::WaitForOp,
                acc,
            })
        })
    }

    // Check if the state described by the accumulator is
    // accepting or not.
    // If we are waiting for an
    fn is_done(&self) -> bool {
        match self.state {
            State::WaitForOp => true,
            State::WaitForVal(_) => false,
        }
    }

    fn finish(self) -> Option<ComputationTree> {
        if self.is_done() {
            Some(self.acc)
        } else {
            None
        }
    }

    fn to_unary(s: &Sym) -> UnOp {
        match s {
            Sym::Len => UnOp::Len,
            Sym::Neg => UnOp::Neg,
            Sym::Iota => UnOp::Iota,
            _ => unreachable!(),
        }
    }

    fn to_binary(s: &Sym) -> BinOp {
        match s {
            Sym::Map => BinOp::Map,
            Sym::Eq => BinOp::Eq,
            Sym::Add => BinOp::Add,
            Sym::And => BinOp::And,
            Sym::Reduce => BinOp::Reduce,
            Sym::Filter => BinOp::Filter,
            _ => unreachable!(),
        }
    }

    fn to_comb(s: &Sym) -> Combinator {
        match s {
            Sym::CombS => Combinator::S,
            Sym::CombK => Combinator::K,
            Sym::CombD => Combinator::D,
            Sym::CombI => Combinator::I,
            _ => unreachable!(),
        }
    }

    /// TODO : captures in lambdas ??
    fn count_variables(prog: &Vec<Sym>) -> u32 {
        prog.iter()
            .map(|s| match s {
                Sym::Var(_) => 1,
                _ => 0,
            })
            .sum()
    }

    /// Convert an arbitrary symbol to a computation tree
    fn to_tree<'a>(s: &Sym) -> Result<ComputationTree, &'static str> {
        match s {
            Sym::CombS | Sym::CombK | Sym::CombD | Sym::CombI => {
                Ok(ComputationTree::CombOp(Self::to_comb(s)))
            }
            Sym::Map | Sym::Eq | Sym::Add | Sym::And | Sym::Or | Sym::Filter | Sym::Reduce => {
                Ok(ComputationTree::BinOpSym(Self::to_binary(s)))
            }
            Sym::Iota | Sym::Len | Sym::Neg => Ok(ComputationTree::UnOpSym(Self::to_unary(s))),
            Sym::Num(n) => Ok(ComputationTree::Lit(Literal::Num(n.clone()))),
            Sym::Var(v) => Ok(ComputationTree::Lit(Literal::Var(v.clone()))),
            Sym::Lambda(prog) => non_linear_check(prog).and_then(|body| {
                Ok(ComputationTree::Lambda {
                    vars: Self::count_variables(prog),
                    body: Box::new(body),
                })
            }),
        }
    }

    /// Given an accumulator and a symbol, next computes a new accumulator
    /// and consume the symbol to extend the current computation tree.
    fn next(self, s: &Sym) -> Result<Accumulator, &'static str> {
        match self.state {
            State::WaitForOp => match s {
                Sym::Map | Sym::Eq | Sym::Filter | Sym::Reduce | Sym::Add | Sym::And | Sym::Or => {
                    Ok(Accumulator {
                        state: State::WaitForVal(Self::to_binary(s)),
                        ..self
                    })
                }
                Sym::Iota | Sym::Len => Ok(Accumulator {
                    state: State::WaitForOp,
                    acc: ComputationTree::UnaryOp {
                        op: Self::to_unary(s),
                        lhs: Box::new(self.acc),
                    },
                }),
                _ => Err("Expected operator"),
            },
            State::WaitForVal(op) => Self::to_tree(s).and_then(|tree| {
                Ok(Accumulator {
                    state: State::WaitForOp,
                    acc: ComputationTree::BinaryOp {
                        op,
                        lhs: Box::new(tree),
                        rhs: Box::new(self.acc),
                    },
                })
            }),
        }
    }
}

// Check that a sequence of symbols is well formed
fn linear_check(prog: &[Sym]) -> Result<ComputationTree, &'static str> {
    let first = Accumulator::new(&prog[0])?;
    let next = &prog[1..];
    next.iter()
        .try_fold(first, |acc, s| acc.next(s))
        .and_then(|a| {
            // println!("debug {:?}", a);
            a.finish().ok_or("Symbols remaining")
        })
}

// Check that a sequence of symbols is well formed (in the context of a lambda)
fn non_linear_check(_prog: &[Sym]) -> Result<ComputationTree, &'static str> {
    Err("TODO: Lambdas not supported")
}

/// Check that an ULP program is well formed and returns its associated
/// computation tree
pub fn check(mut prog: Vec<Sym>) -> Result<ComputationTree, &'static str> {
    if prog.len() == 0 {
        Err("No symbols")
    } else {
        prog.reverse();
        linear_check(&prog)
    }
}

#[cfg(test)]
mod test {
    use parser::Sym;

    use crate::check;

    #[test]
    pub fn test_linear_check1() {
        let prog = vec![
            Sym::Num("1".to_string()),
            Sym::Map,
            Sym::Add,
            Sym::Map,
            Sym::Iota,
            Sym::Num("2".to_string()),
        ];
        let res = check(prog);
        println!("result: {:?}", res);
        assert!(res.is_ok())
    }

    #[test]
    pub fn test_linear_check2() {
        let prog = vec![
            Sym::Map,
            Sym::Add,
            Sym::Map,
            Sym::Iota,
            Sym::Num("2".to_string()),
        ];
        let err = check(prog);
        println!("result: {:?}", err);
        assert!(err.is_err())
    }

    #[test]
    pub fn test_linear_check3() {
        let prog = vec![
            Sym::Add,
            Sym::Reduce,
            Sym::Len,
            Sym::Map,
            Sym::Iota,
            Sym::Num("2".to_string()),
        ];
        let err = check(prog);
        println!("result: {:?}", err);
        assert!(err.is_ok())
    }
}
