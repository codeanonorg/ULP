use ariadne::{Color, Label, ReportKind, Source};
use computations::{
    check,
    types::{self, Type},
};
use parser::{parse, spans, SpannedExt};
use rustyline::{error::ReadlineError, Editor};
use std::ops::Range;

const HISTORYFILE: &str = "/tmp/ulp-repl.history";

type SrcId = (String, Range<usize>);
type Report = ariadne::Report<SrcId>;

fn main() {
    let mut rl = Editor::<()>::new();
    rl.load_history(HISTORYFILE).unwrap_or(());

    loop {
        match rl.readline("ULP> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let (ast, errors) = parse("<repl>", &line);
                for err in errors {
                    err.eprint(("<repl>".to_string(), Source::from(&line)))
                        .unwrap();
                }
                if let Some(ast) = ast {
                    match check(ast) {
                        Ok(comp) => {
                            match Type::infer(comp.as_ref()) {
                                Ok(ty) => println!("{:#?}\n\t-> {}", comp, ty),
                                Err(err) => err.value.labels().into_iter().fold(Report::build(ReportKind::Error, "<repl>", err.span.start)
                                    .with_message("Type error")
                                    .with_label(Label::new(("<repl>".to_string(), err.span)).with_message(err.value).with_color(Color::Red))
                                    .with_note("The expression could not be correctly validated for type consistency"), |r, lspan| r.with_label(Label::new(("<repl>".to_string(), lspan.span)).with_color(Color::Blue).with_message(lspan.value)))
                                    .finish()
                                    .eprint(("<repl>".to_string(), Source::from(&line)))
                                    .unwrap()
                            }
                        },
                        Err(err) => Report::build(ReportKind::Error, "<repl>", err.span.start)
                            .with_label(Label::new(("<repl>".into(), err.span)).with_color(ariadne::Color::Red).with_message(err.value.to_string()))
                            .with_message("Check error")
                            .with_note("The structure is correct, however ULP could not figure out how to compute the expression.")
                            .finish()
                            .eprint(("<repl>".to_string(), Source::from(&line)))
                            .unwrap(),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => break,
            Err(err) => eprintln!("Fatal error: {}", err),
        }
    }

    rl.save_history(HISTORYFILE).unwrap_or(());
}
