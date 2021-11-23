use std::{ops::Deref, rc::Rc};

use ariadne::ReportKind;

use crate::{Position, Positioned};

pub type SourceId = Position;
pub type Label = ariadne::Label<SourceId>;
pub type ReportBuilder = ariadne::ReportBuilder<SourceId>;
pub type Report = ariadne::Report<SourceId>;

impl ariadne::Span for Position {
    type SourceId = Option<Rc<str>>;

    fn source(&self) -> &Self::SourceId {
        &self.reference
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

impl<T: ToString> Positioned<T> {
    pub fn into_report(self, kind: ReportKind) -> ReportBuilder {
        Report::build(kind, self.pos.reference, self.pos.span.start)
            .with_message(self.value.to_string())
    }
}

impl<T: ToString> Into<Label> for Positioned<T> {
    fn into(self) -> Label {
        Label::new(self.pos).with_message(self.value.to_string())
    }
}
