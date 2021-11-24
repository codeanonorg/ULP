use std::{
    error::Error,
    fmt,
    ops::{Deref, Range},
    rc::Rc,
};

pub mod report;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Self {
            start: r.start,
            end: r.end,
        }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

impl Span {
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Reference(Option<Rc<str>>);

impl<'a> From<&'a str> for Reference {
    fn from(s: &'a str) -> Self {
        Self(Some(s.into()))
    }
}

impl From<String> for Reference {
    fn from(s: String) -> Self {
        Self(Some(s.into()))
    }
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = self.0.as_deref() {
            write!(f, "{}", s)
        } else {
            write!(f, "<unknown>")
        }
    }
}

impl Reference {
    pub fn or(self, other: Self) -> Self {
        Self(self.0.or(other.0))
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Position {
    pub reference: Reference,
    pub span: Span,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.reference, self.span)
    }
}

impl Position {
    pub fn spanned<S: Into<Span>>(span: S) -> Self {
        Self {
            span: span.into(),
            ..Default::default()
        }
    }
    pub fn with_reference<S: Into<Reference>>(mut self, reference: S) -> Self {
        self.reference = reference.into();
        self
    }

    pub fn with_span<S: Into<Span>>(mut self, span: S) -> Self {
        self.span = span.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Positioned<T> {
    pub pos: Position,
    pub value: T,
}

impl<T: fmt::Display> fmt::Display for Positioned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pos, self.value)
    }
}

impl<T: Error> Error for Positioned<T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.value.source()
    }
}

impl Position {
    pub fn merge(self, other: Self) -> Self {
        Self {
            reference: self.reference.or(other.reference),
            span: self.span.merge(other.span),
        }
    }
}

impl<T> Deref for Positioned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Positioned<T> {
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Positioned<U> {
        Positioned {
            pos: self.pos,
            value: map(self.value),
        }
    }

    pub fn as_ref(&self) -> Positioned<&T> {
        Positioned {
            pos: self.pos.clone(),
            value: &self.value,
        }
    }

    pub fn as_deref(&self) -> Positioned<&T::Target>
    where
        T: Deref,
    {
        self.as_ref().map(|r| r.deref())
    }

    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn with_reference<S: Into<Reference>>(self, reference: S) -> Self {
        Self {
            pos: self.pos.with_reference(reference),
            value: self.value,
        }
    }

    pub fn with_span<S: Into<Span>>(self, span: S) -> Self {
        Self {
            pos: self.pos.with_span(span),
            value: self.value,
        }
    }
}

impl<T> Positioned<Vec<T>> {
    pub fn from_positioned(values: Vec<Positioned<T>>) -> Self {
        let (pos, values): (Vec<_>, Vec<_>) = values.into_iter().map(|p| (p.pos, p.value)).unzip();
        let pos = pos.into_iter().fold(Position::default(), Position::merge);
        Self { pos, value: values }
    }
}

impl<T, E> Positioned<Result<T, E>> {
    pub fn transpose(self) -> Result<Positioned<T>, Positioned<E>> {
        let Self { pos, value } = self;
        value
            .map(|t| Positioned {
                pos: pos.clone(),
                value: t,
            })
            .map_err(|e| Positioned { pos, value: e })
    }
}

impl<T> Positioned<Option<T>> {
    pub fn transpose(self) -> Option<Positioned<T>> {
        let Self { pos, value } = self;
        value.map(|value| Positioned { pos, value })
    }
}

pub trait PositionedExt: Sized {
    fn positioned(self, pos: Position) -> Positioned<Self> {
        Positioned { pos, value: self }
    }

    fn spanned<S: Into<Span>>(self, span: S) -> Positioned<Self> {
        self.positioned(Position::spanned(span))
    }

    fn referenced<S: Into<Reference>>(self, reference: S) -> Positioned<Self> {
        self.positioned(Position::default().with_reference(reference))
    }
}

impl<T> PositionedExt for T {}
