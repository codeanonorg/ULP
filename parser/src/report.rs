use ariadne::{Color, Fmt, ReportKind};
use chumsky::{error::SimpleReason, prelude::Simple};
use std::rc::Rc;

use crate::token::Token;
use utils::{report::*, Position, PositionedExt, Reference};

pub(crate) fn report_of_char_error(src_id: impl Into<Reference>, err: Simple<char>) -> Report {
    let pos = Position::spanned(err.span()).with_reference(src_id);
    match err.reason() {
        SimpleReason::Unclosed { span, delimiter } => {
            format!("Unclosed delimiter {}", delimiter.fg(Color::Yellow))
                .positioned(pos.clone())
                .into_report(ReportKind::Error)
                .with_label(
                    "Unclosed delimiter is here"
                        .positioned(pos)
                        .into_label()
                        .with_color(Color::Blue),
                )
                .finish()
        }
        SimpleReason::Unexpected => if let Some(found) = err.found() {
            format!("Unexpected input {}", found.fg(Color::Red))
        } else {
            format!("Unexpected input")
        }
        .positioned(pos.clone())
        .into_report(ReportKind::Error)
        .with_label(
            Label::new(pos)
                .with_color(Color::Blue)
                .with_message(format!(
                    "Unexpected token {}",
                    err.expected()
                        .map(|t| t.fg(Color::Cyan).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
        )
        .finish(),
        SimpleReason::Custom(msg) => msg.positioned(pos).into_report(ReportKind::Error).finish(),
    }
}

pub(crate) fn report_of_token_error(src_id: impl Into<Reference>, err: Simple<Token>) -> Report {
    let pos = Position::spanned(err.span()).with_reference(src_id);
    match err.reason() {
        SimpleReason::Unclosed { span, delimiter } => {
            format!("Unclosed delimiter {}", delimiter.fg(Color::Yellow))
                .positioned(pos.clone())
                .into_report(ReportKind::Error)
                .with_label(
                    Label::new(pos)
                        .with_color(Color::Blue)
                        .with_message(format!("Unclosed delimiter is here")),
                )
                .finish()
        }
        SimpleReason::Unexpected => (if let Some(found) = err.found() {
            format!("Unexpected input {}", found.fg(Color::Red))
        } else {
            format!("Unexpected input")
        })
        .positioned(pos.clone())
        .into_report(ReportKind::Error)
        .with_label(
            Label::new(pos)
                .with_color(Color::Blue)
                .with_message(format!(
                    "Expecting one of {}",
                    err.expected()
                        .map(|v| v.fg(Color::Cyan).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
        )
        .finish(),
        SimpleReason::Custom(msg) => msg.positioned(pos).into_report(ReportKind::Error).finish(),
    }
}
