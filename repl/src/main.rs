use ariadne::Source;
use parser::parse;
use rustyline::{Editor, error::ReadlineError};

const HISTORYFILE: &'static str = "/tmp/ulp-repl.history";

fn main() {
    let mut rl = Editor::<()>::new();
    rl.load_history(HISTORYFILE).unwrap_or(());

    loop {
        match rl.readline("ULP> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                let (ast, errors) = parse("<repl>", &line);
                for err in errors {
                    err.eprint(("<repl>".to_string(), Source::from(&line))).unwrap();
                }
                if let Some(ast) = ast {
                    println!("{:?}", ast);
                }
            },
            Err(ReadlineError::Interrupted) => {},
            Err(ReadlineError::Eof) => break,
            Err(err) => eprintln!("Fatal error: {}", err),
        }
    }

    rl.save_history(HISTORYFILE).unwrap_or(());
}
