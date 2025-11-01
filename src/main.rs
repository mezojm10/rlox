use std::{io::Write, process::exit};

use clap::Parser;

enum Opcode {
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,
}

impl Opcode {
    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Return),
            1 => Some(Self::Constant),
            2 => Some(Self::Negate),
            3 => Some(Self::Add),
            4 => Some(Self::Subtract),
            5 => Some(Self::Multiply),
            6 => Some(Self::Divide),
            _ => None,
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

struct VM {
    chunk: Chunk,
    ip: usize,
    stack: [Value; 256],
    sp: u8,
}

impl VM {
    fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: [0.; 256],
            sp: 0,
        }
    }

    fn push(&mut self, value: Value) {
        if self.sp == 255 {
            panic!("Stack overflow!");
        }
        self.stack[self.sp as usize] = value;
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp as usize]
    }

    fn interpret(&mut self, chunk: Chunk) -> InterpretRes {
        self.ip = 0;
        self.chunk = chunk;
        self.run()
    }

    fn run(&mut self) -> InterpretRes {
        loop {
            let instruction = self.chunk.codes[self.ip];
            self.ip += 1;

            match Opcode::from_u8(instruction) {
                Some(Opcode::Return) => {
                    let value = self.pop();
                    println!("{}", value);
                    return InterpretRes::Ok;
                }
                Some(Opcode::Constant) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.push(value);
                    self.ip += 1;
                }
                Some(Opcode::Negate) => {
                    // Negate the top of the stack in-place
                    self.stack[self.sp as usize - 1] = -self.stack[self.sp as usize - 1];
                }
                Some(Opcode::Add) => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(x + y);
                }
                Some(Opcode::Subtract) => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(x - y);
                }
                Some(Opcode::Multiply) => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(x * y);
                }
                Some(Opcode::Divide) => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(x / y);
                }
                None => return InterpretRes::CompileError,
            }
        }
    }
}

enum InterpretRes {
    Ok,
    CompileError,
    RuntimeError,
}

struct Chunk {
    codes: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

type Value = f64;

impl Chunk {
    fn new() -> Self {
        Chunk {
            codes: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    fn add_op(&mut self, code: u8, line: usize) {
        self.codes.push(code);
        self.lines.push(line);
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        loop {
            if offset >= self.codes.len() {
                return;
            }
            print!("{:04}", offset);
            if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
                print!("   | ");
            } else {
                print!("{:4} ", self.lines[offset]);
            }
            offset += {
                match Opcode::from_u8(self.codes[offset]) {
                    Some(Opcode::Constant) => {
                        println!(
                            "{:16} {:4} {}",
                            "OP_CONSTANT",
                            &self.codes[offset + 1],
                            &self.constants[self.codes[offset + 1] as usize]
                        );
                        2
                    }
                    Some(Opcode::Return) => {
                        println!("{:16}", "OP_RETURN");
                        1
                    }
                    Some(Opcode::Negate) => {
                        println!("{:16}", "OP_NEGATE");
                        1
                    }
                    Some(Opcode::Add) => {
                        println!("{:16}", "OP_ADD");
                        1
                    }
                    Some(Opcode::Subtract) => {
                        println!("{:16}", "OP_SUBTRACT");
                        1
                    }
                    Some(Opcode::Multiply) => {
                        println!("{:16}", "OP_MULTIPLY");
                        1
                    }
                    Some(Opcode::Divide) => {
                        println!("{:16}", "OP_DIVIDE");
                        1
                    }
                    None => {
                        println!("{:16}", "Unknown opcode");
                        1
                    }
                }
            };
        }
    }
}

#[derive(Parser)]
struct Args {
    file: Option<std::path::PathBuf>,
}

fn main() {
    let args = Args::parse();

    if let Some(path) = args.file {
        // Run file
        if !path.is_file() {
            println!("Cannot access file: {}", path.to_string_lossy());
            return;
        }
        let content = std::fs::read_to_string(path).unwrap();
        match interpret(&content) {
            InterpretRes::Ok => exit(0),
            InterpretRes::CompileError => exit(65),
            InterpretRes::RuntimeError => exit(70),
        }
    } else {
        // Run Repl
        repl();
    }
}

fn repl() {
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let read_bytes = std::io::stdin().read_line(&mut buffer).unwrap();
        if read_bytes == 0 {
            println!("");
            break;
        }

        interpret(&buffer);
        buffer.clear();
    }
}

fn interpret(content: &str) -> InterpretRes {
    compile(&content);
    InterpretRes::Ok
}

mod scanner;

use scanner::{Scanner, TokenType};

fn compile(content: &str) {
    let mut scanner = Scanner::new(content);
    let mut line: isize = -1;
    loop {
        let token = scanner.scan_token();
        if token.line as isize != line {
            print!("{:04} ", token.line);
            line = token.line as isize;
        } else {
            print!(" | ")
        }
        token.print_info();
        if token.tok_type == TokenType::Eof {
            break;
        }
    }
}
