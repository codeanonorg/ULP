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
pub struct Position {
    pub reference: Option<Rc<str>>,
    pub span: Span,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.reference.as_deref().unwrap_or("<unknown>"),
            self.span
        )
    }
}

impl Position {
    pub fn with_reference<S: Into<Rc<str>>>(mut self, reference: S) -> Self {
        self.reference = Some(reference.into());
        self
    }

    pub fn with_span<S: Into<Span>>(mut self, span: S) -> Self {
        self.span = span.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Positioned<T> {
    pos: Position,
    value: T,
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

    pub fn with_reference<S: Into<Rc<str>>>(self, reference: S) -> Self {
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
    fn positioned<St: Into<Rc<str>>, Sp: Into<Span>>(
        self,
        span: Sp,
        reference: St,
    ) -> Positioned<Self> {
        Positioned {
            pos: Position {
                reference: Some(reference.into()),
                span: span.into(),
            },
            value: self,
        }
    }

    fn spanned<S: Into<Span>>(self, span: S) -> Positioned<Self> {
        Positioned {
            pos: Position::default().with_span(span),
            value: self,
        }
    }

    fn referenced<S: Into<Rc<str>>>(self, reference: S) -> Positioned<Self> {
        Positioned {
            pos: Position {
                reference: Some(reference.into()),
                span: Span::default(),
            },
            value: self,
        }
    }
}

impl<T> PositionedExt for T {}
