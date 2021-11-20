use std::ops::{Range, Deref};

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> From<Spanned<T>> for (T, Span) {
    fn from(s: Spanned<T>) -> Self {
        (s.value, s.span)
    }
}

pub trait SpannedExt: Sized {
    fn spanned(self, span: Span) -> Spanned<Self> {
        Spanned { span, value: self }
    }
}

impl<T> SpannedExt for T {}
