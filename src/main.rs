use clap::Parser;
use miette::{Context, IntoDiagnostic, Result};
use rlox::Lexer;
use std::io::Write;

#[derive(Parser)]
struct Args {
    file: Option<std::path::PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(path) = args.file {
        // Run file
        let content = std::fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("reading '{}' failed", path.display()))?;
        compile(&content);
        Ok(())
    } else {
        // Run Repl
        repl()
    }
}

fn repl() -> Result<()> {
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().into_diagnostic()?;
        let read_bytes = std::io::stdin().read_line(&mut buffer).into_diagnostic()?;
        if read_bytes == 0 {
            println!("");
            break;
        }

        compile(&buffer);
        buffer.clear();
    }

    Ok(())
}

fn compile(content: &str) {
    let mut line: isize = -1;
    for token in Lexer::new(content) {
        let token = match token {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{e:?}");
                continue;
            }
        };
        if token.line as isize != line {
            print!("{:04} ", token.line);
            line = token.line as isize;
        } else {
            print!(" | ")
        }
        println!("{}", token);
    }
}
