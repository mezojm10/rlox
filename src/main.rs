enum Opcode {
    Constant(usize),
    Return,
}

struct Chunk {
    codes: Vec<Opcode>,
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

    fn add_op(&mut self, code: Opcode, line: usize) {
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
        for (i, code) in self.codes.iter().enumerate() {
            print!("{:04}", offset);
            if i > 0 && self.lines[i] == self.lines[i - 1] {
                print!("   | ");
            } else {
                print!("{:4} ", self.lines[i]);
            }
            offset += {
                match &code {
                    Opcode::Constant(v) => {
                        println!("{:16} {:4} {}", "OP_CONSTANT", v, &self.constants[*v]);
                        2
                    }
                    Opcode::Return => {
                        println!("{:16}", "OP_RETURN");
                        1
                    }
                }
            };
        }
    }
}

fn main() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(1.2);
    chunk.add_op(Opcode::Constant(constant_index), 1);
    chunk.add_op(Opcode::Return, 1);

    chunk.disassemble("test chunk");
}
