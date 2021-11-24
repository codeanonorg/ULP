use ariadne::{Color, ReportKind, Source};
use computations::check;
use parser::parse;
use rustyline::{error::ReadlineError, Editor};
use utils::{PositionedExt, Reference};

const HISTORYFILE: &str = "/tmp/ulp-repl.history";

fn main() {
    let reference = Reference::from("<repl>");
    let mut rl = Editor::<()>::new();
    rl.load_history(HISTORYFILE).unwrap_or(());

    loop {
        match rl.readline("ULP> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let (ast, errors) = parse(reference.clone(), &line);
                for err in errors {
                    err.eprint(("<repl>".into(), Source::from(&line))).unwrap();
                }
                if let Some(ast) = ast {
                    match check(ast) {
                        Ok(comp) => println!("Computation {:#?}", comp),
                        Err(err) => "Check error"
                            .positioned(err.pos.clone().with_reference("<repl>"))
                            .into_report(ReportKind::Error)
                            .with_label(err.into_label().with_color(Color::Red))
                            .with_note("The structure is correct, however ULP could not figure out how to compute the expression.")
                            .finish()
                            .eprint((reference.clone(), Source::from(&line)))
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
