#[allow(dead_code)]
mod trees;

use parser::Sym;
use trees::{ComputationTree, Literal, UnOp};
use utils::{Position, Positioned, PositionedExt};

use crate::trees::{BinOp, Combinator};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ComputationError {
    #[error("Expected an operator")]
    ExpectedOperator,
    #[error("Expression is empty")]
    Empty,
    #[error("Expression could not fully be computed")]
    SymbolsRemaining,
    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(&'static str),
}

pub type Error = Positioned<ComputationError>;

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
    fn new(s: Positioned<&Sym>) -> Result<Self, Error> {
        Self::to_tree(s).map(|acc| Accumulator {
            state: State::WaitForOp,
            acc,
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
    fn count_variables<'a, I: IntoIterator<Item = &'a Sym>>(prog: I) -> u32 {
        prog.into_iter()
            .map(|s| match s {
                Sym::Var(_) => 1,
                _ => 0,
            })
            .sum()
    }

    /// Convert an arbitrary symbol to a computation tree
    fn to_tree<'a>(s: Positioned<&Sym>) -> Result<ComputationTree, Error> {
        match s.value {
            Sym::CombS | Sym::CombK | Sym::CombD | Sym::CombI => {
                Ok(ComputationTree::CombOp(Self::to_comb(s.value)))
            }
            Sym::Map | Sym::Eq | Sym::Add | Sym::And | Sym::Or | Sym::Filter | Sym::Reduce => {
                Ok(ComputationTree::BinOpSym(Self::to_binary(s.value)))
            }
            Sym::Iota | Sym::Len | Sym::Neg => {
                Ok(ComputationTree::UnOpSym(Self::to_unary(s.value)))
            }
            Sym::Literal(n) => Ok(ComputationTree::Lit(n.clone().into())),
            Sym::Var(v) => Ok(ComputationTree::Lit(Literal::Var(v.clone()))),
            Sym::Lambda(prog) => non_linear_check(
                prog.iter().map(|s| &s.value).positioned(
                    prog.iter()
                        .map(|s| s.pos.clone())
                        .fold(Position::default(), Position::merge),
                ),
            )
            .map(|body| ComputationTree::Lambda {
                vars: Self::count_variables(prog.iter().map(|s| &s.value)),
                body: Box::new(body),
            }),
        }
    }

    /// Given an accumulator and a symbol, next computes a new accumulator
    /// and consume the symbol to extend the current computation tree.
    fn next(self, s: Positioned<&Sym>) -> Result<Accumulator, Error> {
        match self.state {
            State::WaitForOp => match s.value {
                Sym::Map | Sym::Eq | Sym::Filter | Sym::Reduce | Sym::Add | Sym::And | Sym::Or => {
                    Ok(Accumulator {
                        state: State::WaitForVal(Self::to_binary(s.value)),
                        ..self
                    })
                }
                Sym::Iota | Sym::Len => Ok(Accumulator {
                    state: State::WaitForOp,
                    acc: ComputationTree::UnaryOp {
                        op: Self::to_unary(s.value),
                        lhs: Box::new(self.acc),
                    },
                }),
                _ => Err(ComputationError::ExpectedOperator.positioned(s.pos)),
            },
            State::WaitForVal(op) => Self::to_tree(s).map(|tree| Accumulator {
                state: State::WaitForOp,
                acc: ComputationTree::BinaryOp {
                    op,
                    lhs: Box::new(tree),
                    rhs: Box::new(self.acc),
                },
            }),
        }
    }
}

// Check that a sequence of symbols is well formed
fn linear_check<'a, I: IntoIterator<Item = Positioned<&'a Sym>>>(
    prog: Positioned<I>,
) -> Result<ComputationTree, Error> {
    let mut it = prog.value.into_iter();
    let acc = Accumulator::new(
        it.next()
            .ok_or(ComputationError::Empty.positioned(prog.pos.clone()))?,
    )?;
    let a = it.try_fold(acc, |acc, s| acc.next(s))?;
    a.finish()
        .ok_or(ComputationError::SymbolsRemaining.positioned(prog.pos))
}

// Check that a sequence of symbols is well formed (in the context of a lambda)
fn non_linear_check<'a, I: IntoIterator<Item = &'a Sym>>(
    prog: Positioned<I>,
) -> Result<ComputationTree, Error> {
    Err(ComputationError::UnsupportedFeature("non_linear_check").positioned(prog.pos))
}

/// Check that an ULP program is well formed and returns its associated
/// computation tree
pub fn check(prog: Vec<Positioned<Sym>>) -> Result<ComputationTree, Error> {
    let pos = prog
        .iter()
        .map(|s| s.pos.clone())
        .fold(Position::default(), Position::merge);
    if prog.is_empty() {
        Err(ComputationError::Empty.positioned(pos))
    } else {
        linear_check(prog.iter().map(|s| s.as_ref()).positioned(pos))
    }
}

#[cfg(test)]
mod test {
    use parser::*;
    use utils::PositionedExt;

    use crate::check;

    #[test]
    pub fn test_linear_check1() {
        let prog = vec![
            Sym::Literal(Lit::Num("1".to_string())),
            Sym::Map,
            Sym::Add,
            Sym::Map,
            Sym::Iota,
            Sym::Literal(Lit::Num("2".to_string())),
        ];
        let res = check(
            prog.into_iter()
                .enumerate()
                .map(|(i, s)| s.spanned(i..i + 1))
                .collect(),
        );
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
            Sym::Literal(Lit::Num("2".to_string())),
        ];
        let res = check(
            prog.into_iter()
                .enumerate()
                .map(|(i, s)| s.spanned(i..i + 1))
                .collect(),
        );
        println!("result: {:?}", res);
        assert!(res.is_err())
    }

    #[test]
    pub fn test_linear_check3() {
        let prog = vec![
            Sym::Add,
            Sym::Reduce,
            Sym::Len,
            Sym::Map,
            Sym::Iota,
            Sym::Literal(Lit::Num("2".to_string())),
        ];
        let res = check(
            prog.into_iter()
                .enumerate()
                .map(|(i, s)| s.spanned(i..i + 1))
                .collect(),
        );
        println!("result: {:?}", res);
        assert!(res.is_err())
    }
}
