use ariadne::{Label, ReportKind, Source};
use computations::check;
use parser::parse;
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
                        Ok(comp) => println!("Computation {:#?}", comp),
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
