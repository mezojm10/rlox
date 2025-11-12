pub enum Opcode {
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,
    Not = 7,
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
            7 => Some(Self::Not),
            _ => None,
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

pub struct VM {
    chunk: Chunk,
    ip: usize,
    stack: [Value; 256],
    sp: u8,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: [Value::Nil; 256],
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

    fn peek(&self) -> Value {
        self.stack[self.sp as usize - 1]
    }

    pub fn interpret(&mut self, chunk: Chunk) -> miette::Result<()> {
        self.ip = 0;
        self.chunk = chunk;
        self.run()
    }

    fn run(&mut self) -> miette::Result<()> {
        self.chunk.disassemble("Test Chunk");
        loop {
            let instruction = self.chunk.codes[self.ip];
            self.ip += 1;

            match Opcode::from_u8(instruction) {
                Some(Opcode::Return) => {
                    let value = self.pop();
                    println!("{value}");
                    return Ok(());
                }
                Some(Opcode::Constant) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.push(value);
                    self.ip += 1;
                }
                Some(Opcode::Not) => {
                    let val = self.pop();
                    match val {
                        Value::Bool(b) => self.push(Value::Bool(!b)),
                        _ => {
                            return Err(miette::miette!(
                                "Logical NOT operation requires a boolean."
                            ))
                        }
                    }
                }
                Some(Opcode::Negate) => {
                    // Negate the top of the stack in-place
                    let val = self.peek();
                    if let Value::Number(n) = val {
                        self.stack[self.sp as usize - 1] = Value::Number(-n);
                    } else {
                        return Err(miette::miette!("Negation must be a number."));
                    }
                }
                Some(Opcode::Add) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a + b));
                        }
                        _ => return Err(miette::miette!("Addition requires two numbers.")),
                    }
                }
                Some(Opcode::Subtract) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a - b));
                            continue;
                        }
                        _ => return Err(miette::miette!("Subtraction requires two numbers.")),
                    }
                }
                Some(Opcode::Multiply) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a * b));
                            continue;
                        }
                        _ => return Err(miette::miette!("Multiplication requires two numbers.")),
                    }
                }
                Some(Opcode::Divide) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a / b));
                            continue;
                        }
                        _ => return Err(miette::miette!("Division requires two numbers.")),
                    }
                }
                None => return Err(miette::miette!("Unknown opcode encountered.")),
            }
        }
    }
}

pub struct Chunk {
    codes: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            codes: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn emit_op(&mut self, code: Opcode, line: usize) {
        self.codes.push(code.into());
        self.lines.push(line);
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn emit_constant(&mut self, value: Value, line: usize) -> miette::Result<()> {
        // TODO: Handle constant index overflow
        let constant_index = self.add_constant(value);
        if constant_index > u8::MAX as usize {
            return Err(miette::miette!("Constant index overflow."));
        }
        self.emit_op(Opcode::Constant, line);
        self.codes.push(constant_index as u8);
        self.lines.push(line);
        Ok(())
    }

    pub fn emit_return(&mut self, line: usize) {
        self.emit_op(Opcode::Return, line);
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
            offset += self.disassemble_op(offset);
        }
    }

    fn disassemble_op(&self, offset: usize) -> usize {
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
            Some(Opcode::Not) => {
                println!("{:16}", "OP_NOT");
                1
            }
            None => {
                println!("{:16}", "Unknown opcode");
                1
            }
        }
    }
}

use std::fmt;
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}
