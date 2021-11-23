use parser::Sym;
use std::{borrow::Borrow, collections::HashMap, fmt};

use parser::{Span, Spanned, SpannedExt};

use crate::trees::{BinOp, Combinator, ComputationTree, Literal, UnOp};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Type mismatch")]
    TypeMismatch {
        actual: Spanned<Type>,
        expected: Spanned<Type>,
    },
    #[error("Type is not fully known")]
    UnexpectedUnknown(Span),
    #[error("Cannot instanciate infinite type")]
    InfiniteType(Spanned<u32>, Spanned<Type>),
}

type Error = Spanned<TypeError>;
type Result<T> = std::result::Result<T, Error>;

impl TypeError {
    pub fn labels(&self) -> Vec<Spanned<String>> {
        match self {
            Self::TypeMismatch { actual, expected } => vec![
                format!("This is of type `{}`", actual.value).spanned(actual.span.clone()),
                format!("This is of type `{}`", expected.value).spanned(expected.span.clone()),
            ],
            Self::UnexpectedUnknown(span) => {
                vec!["Type is unknown here".to_string().spanned(span.clone())]
            }
            Self::InfiniteType(var, ty) => {
                vec!["Type variable is here".to_string().spanned(var.span.clone()), 
                format!("Variable `t{}` appears in this type and would lead to an inifinitely large type", var.value).spanned(ty.span.clone())]
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Var(u32),
    Unknown,
    App {
        arg: Spanned<Box<Self>>,
        ret: Spanned<Box<Self>>,
    },
    List(Spanned<Box<Self>>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Var(v) => write!(f, "t{}", v),
            Self::Unknown => write!(f, "??"),
            Self::List(inner) => write!(f, "{} list", inner.value),
            Self::App { arg, ret } if arg.is_app() => write!(f, "({}) -> {}", arg.value, ret.value),
            Self::App { arg, ret } => write!(f, "{} -> {}", arg.value, ret.value),
        }
    }
}

impl Type {
    pub fn list(inner: Self) -> Self {
        Self::List(Box::new(inner).spanned(0..0))
    }

    pub fn slist(inner: Spanned<Self>) -> Self {
        Self::List(inner.map(Box::new))
    }

    pub fn app(arg: Self, ret: Self) -> Self {
        Self::App {
            arg: Box::new(arg).spanned(0..0),
            ret: Box::new(ret).spanned(0..0),
        }
    }

    pub fn sapp(arg: Spanned<Self>, ret: Spanned<Self>) -> Self {
        Self::App {
            arg: arg.map(Box::new),
            ret: ret.map(Box::new),
        }
    }

    pub fn app_with(self, span: Span, other: Spanned<Self>) -> Result<Self> {
        match self {
            Self::App { arg, ret } => arg.value.unify(arg.span, other).map(|_| *ret.value),
            ty => Err(TypeError::TypeMismatch {
                actual: ty.spanned(span.clone()),
                expected: Self::app(Self::Var(0), Self::Var(1)).spanned(0..0),
            }
            .spanned(span)),
        }
    }

    pub fn unify(self, span: Span, other: Spanned<Self>) -> Result<Self> {
        match self {
            Self::Var(v) => {
                let m = HashMap::from_iter(vec![(v, other.value.clone())]);
                Ok(other.value.replace(m))
            }
            Self::List(a) => match other.value {
                Self::List(b) => a.value.unify(a.span, b.map(|b| *b)),
                _ => Err(TypeError::TypeMismatch {
                    actual: other,
                    expected: Self::List(a).spanned(span.clone()),
                }
                .spanned(span)),
            },
            Self::App { arg, ret } => match other.value {
                Self::App {
                    arg: other_arg,
                    ret: other_ret,
                } => Ok(Self::sapp(
                    arg.value.unify(arg.span.clone(), other_arg.map(|b| *b))?.spanned(arg.span),
                    ret.value
                        .unify(ret.span.clone(), other_ret.map(|b| *b))?
                        .spanned(ret.span),
                )),
                _ => Err(TypeError::TypeMismatch {
                    actual: other,
                    expected: Self::App { arg, ret }.spanned(span.clone()),
                }
                .spanned(span)),
            },
            Self::Unknown => Err(TypeError::UnexpectedUnknown(span.clone()).spanned(span)),
            a => match other.value {
                Self::Unknown => Err(TypeError::UnexpectedUnknown(other.span).spanned(span)),
                b => {
                    let b = b.unify(other.span.clone(), a.clone().spanned(span.clone()).clone())?;
                    if b == a {
                        Ok(a)
                    } else {
                        Err(TypeError::TypeMismatch { actual: b.spanned(other.span), expected: a.spanned(span.clone()) }.spanned(span))
                    }
                }
            },
        }
    }

    pub fn replace(self, mut replacements: HashMap<u32, Self>) -> Self {
        match self {
            Self::Var(v) => {
                if let Some(ty) = replacements.remove(&v) {
                    ty
                } else {
                    Self::Var(v)
                }
            }
            Self::List(inner) => Self::slist(inner.map(|s| s.replace(replacements))),
            Self::App { arg, ret } => Self::sapp(
                arg.map(|arg| arg.replace(replacements.clone())),
                ret.map(|r| r.replace(replacements)),
            ),
            a => a,
        }
    }

    pub fn infer(tree: Spanned<&ComputationTree>) -> Result<Self> {
        match tree.value {
            ComputationTree::BinOpSym(binop) => Ok(Self::infer_binop(binop)),
            ComputationTree::UnOpSym(unop) => Ok(Self::infer_unop(unop)),
            ComputationTree::CombOp(comb) => Ok(Self::infer_combinator(comb)),
            ComputationTree::Lit(lit) => Self::infer_literal(lit.spanned(tree.span)),
            ComputationTree::BinaryOp { op, lhs, rhs } => Self::infer_binop(op)
                .app_with(
                    tree.span.clone(),
                    Self::infer(lhs.as_deref())?.spanned(lhs.span.clone()),
                )?
                .app_with(
                    lhs.span.clone(),
                    Self::infer(rhs.as_deref())?.spanned(rhs.span.clone()),
                ),
            ComputationTree::UnaryOp { op, lhs } => Self::infer_unop(op).app_with(
                tree.span,
                Self::infer(lhs.as_deref())?.spanned(lhs.span.clone()),
            ),
            ComputationTree::Lambda { .. } => Ok(Self::Unknown),
            ComputationTree::App { lhs, rhs } => Self::infer(lhs.as_deref())?.app_with(
                lhs.span.clone(),
                Self::infer(rhs.as_deref())?.spanned(rhs.span.clone()),
            ),
        }
    }

    pub fn infer_literal(lit: Spanned<&Literal>) -> Result<Self> {
        match lit.value {
            Literal::True | Literal::False => Ok(Self::Bool),
            Literal::Num(_) => Ok(Self::Int),
            Literal::Var(_) => Ok(Self::Unknown),
            Literal::Table(v) => {
                let mut it = v.iter();
                if let Some(first_lit) = it.next() {
                    let inner = it.try_fold(Self::infer_literal(first_lit.as_ref())?, |ty, l| {
                        let tty = Self::infer_literal(l.as_ref())?;
                        if tty == ty {
                            Ok(ty)
                        } else {
                            Err(TypeError::TypeMismatch {
                                actual: tty.spanned(l.span.clone()),
                                expected: ty.spanned(first_lit.span.clone()),
                            }
                            .spanned(lit.span.clone()))
                        }
                    })?;
                    Ok(Self::slist(first_lit.as_ref().map(|_| inner)))
                } else {
                    Ok(Self::list(Self::Var(0)))
                }
            }
        }
    }

    pub fn infer_unop(op: &UnOp) -> Self {
        match op {
            UnOp::Len => Self::app(Self::list(Self::Var(0)), Self::Int),
            UnOp::Neg => Self::app(Self::Bool, Self::Bool),
            UnOp::Iota => Self::app(Self::Int, Self::list(Self::Int)),
        }
    }

    pub fn infer_binop(op: &BinOp) -> Self {
        match op {
            BinOp::Map => Self::app(
                Self::app(Self::Var(0), Self::Var(1)),
                Self::app(Self::list(Self::Var(0)), Self::list(Self::Var(1))),
            ),
            BinOp::Filter => Self::app(
                Self::app(Self::Var(0), Self::Bool),
                Self::app(Self::list(Self::Var(0)), Self::list(Self::Var(0))),
            ),
            BinOp::Reduce => Self::app(
                Self::app(Self::Var(0), Self::app(Self::Var(0), Self::Var(0))),
                Self::app(Self::list(Self::Var(0)), Self::Var(0)),
            ),
            BinOp::Eq => Self::app(Self::Var(0), Self::app(Self::Var(0), Self::Bool)),
            BinOp::Add => Self::app(Self::Int, Self::app(Self::Int, Self::Int)),
            BinOp::And | BinOp::Or => Self::app(Self::Bool, Self::app(Self::Bool, Self::Bool)),
        }
    }

    pub fn infer_combinator(c: &Combinator) -> Self {
        match c {
            Combinator::I => Self::app(Self::Var(0), Self::Var(0)),
            Combinator::D => {
                let x = Self::Var(0);
                let f = Self::app(Self::Var(1), Self::app(Self::Var(2), Self::Var(3)));
                let g = Self::app(x.clone(), Self::Var(1));
                let h = Self::app(x.clone(), Self::Var(2));
                Self::app(f, Self::app(g, Self::app(h, Self::app(x, Self::Var(3)))))
            }
            Combinator::K => Self::app(Self::Var(0), Self::app(Self::Var(1), Self::Var(0))),
            Combinator::S => {
                let x = Self::Var(0);
                let f = Self::app(Self::Var(1), Self::app(x.clone(), Self::Var(2)));
                let g = Self::app(x.clone(), Self::Var(1));
                Self::app(f, Self::app(g, Self::app(x, Self::Var(2))))
            }
        }
    }

    fn is_app(&self) -> bool {
        match self {
            Self::App { .. } => true,
            _ => false,
        }
    }

    fn num_variables(&self) -> u32 {
        match self {
            Self::Var(_) => 1,
            Self::List(l) => l.num_variables(),
            Self::App { arg, ret } => arg.num_variables() + ret.num_variables(),
            _ => 0,
        }
    }
}
