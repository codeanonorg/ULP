use std::fmt;

/// Type of literals
/// Literals are constant values
#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    True,
    False,
    Num(String),
    Var(u32),
    Table(Vec<Literal>),
}

/// Type of binary operation symbols
#[derive(Clone, PartialEq, Debug)]
pub enum BinOp {
    Map,
    Filter,
    Reduce,
    Eq,
    Add,
    And,
    Or,
}

/// Type of unary operation symbols
#[derive(Clone, PartialEq, Debug)]
pub enum UnOp {
    Len,
    Neg,
    Iota,
}

/// Type of combinator symbols
#[derive(Clone, PartialEq, Debug)]
pub enum Combinator {
    S,
    K,
    I,
    D,
}

/// Type of Computation Trees
/// A computation tree represents a structured sequence
/// of computations to perform
#[derive(Clone, Debug)]
pub enum ComputationTree {
    BinOpSym(BinOp),
    UnOpSym(UnOp),
    CombOp(Combinator),
    Lit(Literal),
    BinaryOp {
        op: BinOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    UnaryOp {
        op: UnOp,
        lhs: Box<Self>,
    },
    Lambda {
        vars: u32,
        body: Box<ComputationTree>,
    },
    App {
        lhs: Box<ComputationTree>,
        rhs: Box<ComputationTree>,
    },
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Num(n) => write!(f, "{}", n),
            Literal::Table(v) => {
                write!(f, "[")?;
                if v.len() != 0 {
                    write!(f, "{}", v[0])?;
                    for i in &v[1..] {
                        write!(f, " {}", i)?;
                    }
                }
                write!(f, "]")
            }
            Literal::Var(u) => write!(f, "x{}", u),
        }
    }
}
