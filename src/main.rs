enum Opcode {
    Return = 0,
    Constant = 1,
}

impl Opcode {
    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Return),
            1 => Some(Self::Constant),
            _ => None,
        }
    }
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
                    None => {
                        println!("{:16}", "Unknown opcode");
                        1
                    }
                }
            };
        }
    }
}

fn main() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(1.2) as u8;
    chunk.add_op(Opcode::Constant as u8, 1);
    chunk.add_op(constant_index, 1);
    chunk.add_op(Opcode::Return as u8, 1);

    chunk.disassemble("test chunk");
}
