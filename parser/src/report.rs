use ariadne::{Color, Fmt};
use chumsky::{error::SimpleReason, prelude::Simple};
use std::ops::Range;

use crate::token::Token;

pub type SrcId = (String, Range<usize>);
pub type Report = ariadne::Report<SrcId>;
pub type Label = ariadne::Label<SrcId>;

pub(crate) fn report_of_char_error(src_id: impl Into<String>, err: Simple<char>) -> Report {
    let id = (src_id.into(), err.span());
    let report = Report::build(ariadne::ReportKind::Error, id.0.clone(), err.span().start);
    match err.reason() {
        SimpleReason::Unclosed { span, delimiter } => report
            .with_message(format!(
                "Unclosed delimiter {}",
                delimiter.fg(Color::Yellow)
            ))
            .with_label(
                Label::new((id.0.clone(), span.clone()))
                    .with_color(Color::Blue)
                    .with_message(format!("Unclosed delimiter is here")),
            )
            .finish(),
        SimpleReason::Unexpected => report
            .with_message(if let Some(found) = err.found() {
                format!("Unexpected input {}", found.fg(Color::Red))
            } else {
                format!("Unexpected input")
            })
            .with_label(Label::new(id).with_color(Color::Blue).with_message(format!(
                    "Unexpected token {}",
                    err.expected()
                        .map(|t| t.fg(Color::Cyan).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))
            .finish(),
        SimpleReason::Custom(msg) => report
            .with_message(msg)
            .finish(),
    }
}

pub(crate) fn report_of_token_error(src_id: impl Into<String>, err: Simple<Token>) -> Report {
    let id = (src_id.into(), err.span());
    let report = Report::build(ariadne::ReportKind::Error, id.0.clone(), err.span().start);
    match err.reason() {
        SimpleReason::Unclosed { span, delimiter } => report
            .with_message(format!(
                "Unclosed delimiter {}",
                delimiter.fg(Color::Yellow)
            ))
            .with_label(
                Label::new((id.0.clone(), span.clone()))
                    .with_color(Color::Blue)
                    .with_message(format!("Unclosed delimiter is here")),
            )
            .finish(),
        SimpleReason::Unexpected => report
            .with_message(if let Some(found) = err.found() {
                format!("Unexpected input {}", found.fg(Color::Red))
            } else {
                format!("Unexpected input")
            })
            .with_label(Label::new(id).with_color(Color::Blue).with_message(format!(
                    "Expecting one of {}",
                    err.expected()
                        .map(|v| v.fg(Color::Cyan).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )))
            .finish(),
        SimpleReason::Custom(msg) => report
            .with_message(msg)
            .finish(),
    }
}
