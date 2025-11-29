use clap::Parser;
use miette::{Context, IntoDiagnostic, Result};
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
        let mut vm = rlox::VM::new();
        compile(&mut vm, &content)?;
        Ok(())
    } else {
        // Run Repl
        repl()
    }
}

fn repl() -> Result<()> {
    let mut buffer = String::new();
    let mut vm = rlox::VM::new();
    loop {
        print!("> ");
        std::io::stdout().flush().into_diagnostic()?;
        let read_bytes = std::io::stdin().read_line(&mut buffer).into_diagnostic()?;
        if read_bytes == 0 {
            println!("");
            break;
        }

        compile(&mut vm, &buffer)?;

        buffer.clear();
    }

    Ok(())
}

fn compile(vm: &mut rlox::VM, content: &str) -> Result<()> {
    rlox::Parser::new(content).parse(vm)?;
    vm.chunk.emit_return(0);
    // Interpret the bytecode
    vm.interpret()
}
