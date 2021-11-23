use std::{error::Error, fmt::{self, Display}, ops::{Range, Deref}};

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

impl<T: Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        write!(f, "{}:{}: {}", self.span.start, self.span.end, self.value)
    }
}

impl<T: Error> Error for Spanned<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.value.source()
    }
}

impl<T> Spanned<T> {
    pub fn as_ref(&self) -> Spanned<&T> {
        (&self.value).spanned(self.span.clone())
    }

    pub fn as_deref(&self) -> Spanned<&T::Target> where T: std::ops::Deref {
        self.value.deref().spanned(self.span.clone())
    }

    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        map(self.value).spanned(self.span)
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Spanned<T>, E> {
        self.value.map(|t| t.spanned(self.span))
    }
}

pub fn spans<T>(spans: &[Spanned<T>]) -> Span {
    match spans.len() {
        0 => 0..0,
        _ => {
            let first = spans.first().map(|s| s.span.start).unwrap();
            let last = spans.last().map(|s| s.span.end).unwrap();
            first..last
        }
    }
}

pub trait SpannedExt: Sized {
    fn spanned(self, span: Span) -> Spanned<Self> {
        Spanned { span, value: self }
    }
}

impl<T> SpannedExt for T {}
