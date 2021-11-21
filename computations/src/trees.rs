use std::fmt;

/// Type of reserved symbols used in ULP
#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    True,
    False,
    Num(String),
    Var(u32),
    Table(Vec<Literal>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinOp {
    Map,
    Eq,
    Add,
    And,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub enum UnOp {
    Len,
    Neg,
    Iota,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Combinator {
    S,
    K,
    I,
    D,
}

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
