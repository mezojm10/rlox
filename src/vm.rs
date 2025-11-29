use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ptr::NonNull;

pub enum Opcode {
    Return = 0,
    Constant = 1,
    Negate = 2,
    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,
    Not = 7,
    True = 8,
    False = 9,
    Nil = 10,
    Less = 11,
    Equal = 12,
    Greater = 13,
    ConstantU16 = 14,
    Print = 15,
    Pop = 16,
    DefineGlobal = 17,
    DefineGlobalU16 = 18,
    GetGlobal = 19,
    GetGlobalU16 = 20,
    SetGlobal = 21,
    SetGlobalU16 = 22,
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
            8 => Some(Self::True),
            9 => Some(Self::False),
            10 => Some(Self::Nil),
            11 => Some(Self::Less),
            12 => Some(Self::Equal),
            13 => Some(Self::Greater),
            14 => Some(Self::ConstantU16),
            15 => Some(Self::Print),
            16 => Some(Self::Pop),
            17 => Some(Self::DefineGlobal),
            18 => Some(Self::DefineGlobalU16),
            19 => Some(Self::GetGlobal),
            20 => Some(Self::GetGlobalU16),
            21 => Some(Self::SetGlobal),
            22 => Some(Self::SetGlobalU16),
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
    pub chunk: Chunk,
    ip: usize,
    stack: [Value; 256],
    sp: u8,
    strings: HashSet<Box<str>>,
    globals: HashMap<String, Value>,
    pub objects: Option<NonNull<Object>>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: [Value::Nil; 256],
            sp: 0,
            objects: None,
            strings: HashSet::new(),
            globals: HashMap::new(),
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

    // Allocate a string object
    // This functions uses string interning, meaning that identical strings share the same memory
    pub fn allocate_string(&mut self, string: &str) -> NonNull<Object> {
        // Check if the string is already interned
        let string = match self.strings.get(string) {
            Some(s) => s,
            None => {
                // If not, intern it and get a reference
                self.strings.insert(string.into());
                self.strings.get(string).unwrap()
            }
        };

        // Create the object and box it
        let boxed = Box::new(Object {
            kind: ObjectKind::String(string.as_ref().into()),
            next: self.objects,
        });

        // Get a raw pointer to the boxed object
        let ptr = NonNull::new(Box::into_raw(boxed)).expect("Box::into_raw returned null");

        // Update the head of the linked list
        self.objects = Some(ptr);

        ptr
    }

    pub fn emit_string(&mut self, string: &str, line: usize) -> miette::Result<()> {
        let string_object = self.allocate_string(string);
        self.chunk.emit_constant(Value::Object(string_object), line)
    }

    pub fn emit_define_global(&mut self, var_name: &str, line: usize) -> miette::Result<()> {
        let string_object = Value::Object(self.allocate_string(var_name));
        self.chunk.emit_two_or_three_bytes(
            string_object,
            line,
            Opcode::DefineGlobal,
            Opcode::DefineGlobalU16,
        )
    }

    pub fn emit_get_global(&mut self, var_name: &str, line: usize) -> miette::Result<()> {
        let string_object = Value::Object(self.allocate_string(var_name));
        self.chunk.emit_two_or_three_bytes(
            string_object,
            line,
            Opcode::GetGlobal,
            Opcode::GetGlobalU16,
        )
    }

    pub fn emit_set_global(&mut self, var_name: &str, line: usize) -> miette::Result<()> {
        let string_object = Value::Object(self.allocate_string(var_name));
        self.chunk.emit_two_or_three_bytes(
            string_object,
            line,
            Opcode::SetGlobal,
            Opcode::SetGlobalU16,
        )
    }

    pub fn interpret(&mut self) -> miette::Result<()> {
        self.chunk.disassemble("Test Chunk");
        let err = |msg: &str, line: usize| Err(miette::miette!("[line {line}] {msg}"));
        loop {
            let instruction = self.chunk.codes[self.ip];
            self.ip += 1;

            match Opcode::from_u8(instruction) {
                Some(Opcode::Return) => {
                    return Ok(());
                }
                Some(Opcode::Pop) => {
                    self.pop();
                }
                Some(Opcode::Print) => {
                    let value = self.pop();
                    println!("{value}");
                }
                Some(Opcode::Constant) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.push(value);
                    self.ip += 1;
                }
                Some(Opcode::ConstantU16) => {
                    let high_byte = self.chunk.codes[self.ip] as u16;
                    let low_byte = self.chunk.codes[self.ip + 1] as u16;
                    let constant_index = (high_byte << 8) | low_byte;
                    let value = self.chunk.constants[constant_index as usize];
                    self.push(value);
                    self.ip += 2;
                }
                Some(Opcode::DefineGlobal) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.define_global(value)?;
                    self.ip += 1;
                }
                Some(Opcode::DefineGlobalU16) => {
                    let high_byte = self.chunk.codes[self.ip] as u16;
                    let low_byte = self.chunk.codes[self.ip + 1] as u16;
                    let constant_index = (high_byte << 8) | low_byte;
                    let value = self.chunk.constants[constant_index as usize];
                    self.define_global(value)?;
                    self.ip += 2;
                }
                Some(Opcode::GetGlobal) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.get_global(value)?;
                    self.ip += 1;
                }
                Some(Opcode::GetGlobalU16) => {
                    let high_byte = self.chunk.codes[self.ip] as u16;
                    let low_byte = self.chunk.codes[self.ip + 1] as u16;
                    let constant_index = (high_byte << 8) | low_byte;
                    let value = self.chunk.constants[constant_index as usize];
                    self.get_global(value)?;
                    self.ip += 2;
                }
                Some(Opcode::SetGlobal) => {
                    let value = self.chunk.constants[self.chunk.codes[self.ip] as usize];
                    self.set_global(value)?;
                    self.ip += 1;
                }
                Some(Opcode::SetGlobalU16) => {
                    let high_byte = self.chunk.codes[self.ip] as u16;
                    let low_byte = self.chunk.codes[self.ip + 1] as u16;
                    let constant_index = (high_byte << 8) | low_byte;
                    let value = self.chunk.constants[constant_index as usize];
                    self.set_global(value)?;
                    self.ip += 2;
                }
                Some(Opcode::True) => {
                    self.push(Value::Bool(true));
                }
                Some(Opcode::False) => {
                    self.push(Value::Bool(false));
                }
                Some(Opcode::Nil) => {
                    self.push(Value::Nil);
                }
                Some(Opcode::Not) => {
                    let val = self.pop();
                    match val {
                        Value::Bool(b) => self.push(Value::Bool(!b)),
                        Value::Nil => self.push(Value::Bool(true)),
                        _ => {
                            return err(
                                format!("Operand for NOT must be a boolean, got {}", val.kind())
                                    .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
                    }
                }
                Some(Opcode::Negate) => {
                    // Negate the top of the stack in-place
                    let val = self.peek();
                    if let Value::Number(n) = val {
                        self.stack[self.sp as usize - 1] = Value::Number(-n);
                    } else {
                        return err(
                            format!("Negation must be a number, got {}", val.kind()).as_str(),
                            self.chunk.lines[self.ip - 1],
                        );
                    }
                }
                Some(Opcode::Add) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a + b));
                        }
                        (Value::Object(a), Value::Object(b)) => {
                            let new_str = self.concatenate_strings(a, b)?;
                            self.push(Value::Object(new_str));
                        }
                        _ => {
                            return err(
                                format!(
                                    "Both operands of + must be strings or numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
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
                        _ => {
                            return err(
                                format!(
                                    "Subtraction requires two numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
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
                        _ => {
                            return err(
                                format!(
                                    "Multiplication requires two numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
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
                        (x, y) => {
                            return err(
                                format!(
                                    "Division requires two numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
                    }
                }
                Some(Opcode::Equal) => {
                    let y = self.pop();
                    let x = self.pop();
                    self.push(Value::Bool(x == y));
                }
                Some(Opcode::Greater) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Bool(a > b));
                        }
                        _ => {
                            return err(
                                format!(
                                    "Greater comparison requires two numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
                    }
                }
                Some(Opcode::Less) => {
                    let y = self.pop();
                    let x = self.pop();
                    match (x, y) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Bool(a < b));
                        }
                        _ => {
                            return err(
                                format!(
                                    "Less comparison requires two numbers, got {} and {}",
                                    x.kind(),
                                    y.kind()
                                )
                                .as_str(),
                                self.chunk.lines[self.ip - 1],
                            )
                        }
                    }
                }
                None => return err("Unknown opcode encountered.", self.chunk.lines[self.ip - 1]),
            }
        }
    }

    fn define_global(&mut self, value: Value) -> miette::Result<()> {
        let err = |msg: &str, line: usize| Err(miette::miette!("[line {line}] {msg}"));
        // Make sure value is string object
        if let Value::Object(obj) = value {
            if let ObjectKind::String(s) = unsafe { &obj.as_ref().kind } {
                let key = unsafe { s.as_ref().to_string() };
                self.globals.insert(key, self.peek());
                self.pop();
                return Ok(());
            }
        }

        err(
            "Got an OP_DEFINE_GLOBAL without a string identifier",
            self.chunk.lines[self.ip - 1],
        )
    }

    fn get_global(&mut self, value: Value) -> miette::Result<()> {
        let err = |msg: &str, line: usize| Err(miette::miette!("[line {line}] {msg}"));
        // Make sure value is string object
        if let Value::Object(obj) = value {
            if let ObjectKind::String(s) = unsafe { &obj.as_ref().kind } {
                let key = unsafe { s.as_ref() };
                let val = match self.globals.get(key) {
                    Some(v) => *v,
                    None => {
                        return err(
                            &format!("Undefined variable: {key}"),
                            self.chunk.lines[self.ip - 1],
                        )
                    }
                };
                self.push(val);
                return Ok(());
            }
        }

        err(
            "Got an OP_GET_GLOBAL without a string identifier",
            self.chunk.lines[self.ip - 1],
        )
    }

    fn set_global(&mut self, value: Value) -> miette::Result<()> {
        let err = |msg: &str, line: usize| Err(miette::miette!("[line {line}] {msg}"));
        // Make sure value is string object
        if let Value::Object(obj) = value {
            if let ObjectKind::String(s) = unsafe { &obj.as_ref().kind } {
                let key = unsafe { s.as_ref() };
                // Set the global
                self.globals
                    .insert(key.to_string(), self.peek())
                    // If the global wasn't already set, return an undefined var error
                    .map_or_else(
                        || {
                            err(
                                &format!("Undefined variable: {key}"),
                                self.chunk.lines[self.ip - 1],
                            )
                        },
                        |_old_val| Ok(()),
                    )?;
                // Pop the new value from the stack
                self.pop();
                return Ok(());
            }
        }

        err(
            "Got an OP_SET_GLOBAL without a string identifier",
            self.chunk.lines[self.ip - 1],
        )
    }

    fn concatenate_strings(
        &mut self,
        a: NonNull<Object>,
        b: NonNull<Object>,
    ) -> miette::Result<NonNull<Object>> {
        let (mut a_str, b_str) = match (&unsafe { a.as_ref() }.kind, &unsafe { b.as_ref() }.kind) {
            (ObjectKind::String(s1), ObjectKind::String(s2)) => {
                let s1 = unsafe { s1.as_ref().to_string() };
                let s2 = unsafe { s2.as_ref() };
                (s1, s2)
            }
            (x, y) => {
                return Err(miette::miette!(
                    "Both operands of + must be strings or numbers, got {} and {}",
                    x.kind(),
                    y.kind()
                ));
            }
        };
        a_str.push_str(b_str);
        Ok(self.allocate_string(&a_str))
    }
}

// Cleanup data allocated by VM
impl Drop for VM {
    fn drop(&mut self) {
        let mut current = self.objects;

        while let Some(ptr) = current {
            // Read the next pointer before dropping the current object
            let boxed = unsafe { Box::from_raw(ptr.as_ptr()) };
            let next = boxed.next;

            // Explicitly drop the boxed object
            drop(boxed);

            // Move to the next object
            current = next;
        }

        self.objects = None;
    }
}

pub struct Chunk {
    codes: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Object(NonNull<Object>),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            // Compare object kinds for equality
            // TODO: We only have strings so this makes sense for now, but in the future we may want to compare object contents
            (Self::Object(l0), Self::Object(r0)) => unsafe { l0.as_ref().kind == r0.as_ref().kind },
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Object {
    kind: ObjectKind,
    next: Option<NonNull<Object>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectKind {
    String(NonNull<str>),
}

impl Value {
    fn kind(&self) -> &str {
        match self {
            Value::Number(_) => "Number",
            Value::Bool(_) => "Bool",
            Value::Nil => "Nil",
            Value::Object(object) => unsafe { object.as_ref().kind.kind() },
        }
    }
}

impl ObjectKind {
    fn kind(&self) -> &str {
        match self {
            ObjectKind::String(_) => "String",
        }
    }
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
        self.codes.push(u8::from(code));
        self.lines.push(line);
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn emit_constant(&mut self, value: Value, line: usize) -> miette::Result<()> {
        self.emit_two_or_three_bytes(value, line, Opcode::Constant, Opcode::ConstantU16)
    }

    fn emit_two_or_three_bytes(
        &mut self,
        value: Value,
        line: usize,
        opcode: Opcode,
        opcode_u16: Opcode,
    ) -> miette::Result<()> {
        let constant_index = self.add_constant(value);
        if constant_index > u8::MAX as usize {
            if constant_index > u16::MAX as usize {
                return Err(miette::miette!("Constant index overflow."));
            }
            // Use u16 opcode
            self.emit_op(opcode_u16, line);
            let high_byte = ((constant_index as u16) >> 8) as u8;
            let low_byte = (constant_index as u16 & 0xFF) as u8;
            self.codes.push(high_byte);
            self.codes.push(low_byte);
        } else {
            // Use u8 opcode
            self.emit_op(opcode, line);
            self.codes.push(constant_index as u8);
        }
        self.lines.push(line);
        Ok(())
    }

    pub fn emit_return(&mut self, line: usize) {
        self.emit_op(Opcode::Return, line);
    }

    /// For debugging purposes. Prints the opcodes in the vm
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

    /// For debugging purposes, prints the operation and its operands
    /// and returns the byte size of the op.
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
            Some(Opcode::ConstantU16) => {
                let high_byte = self.codes[offset + 1] as u16;
                let low_byte = self.codes[offset + 2] as u16;
                let constant_index = (high_byte << 8) | low_byte;
                println!(
                    "{:16} {:4} {}",
                    "OP_CONSTANT_U16", constant_index, &self.constants[constant_index as usize]
                );
                3
            }
            Some(Opcode::DefineGlobal) => {
                println!(
                    "{:16} {:4} {}",
                    "OP_DEFINE_GLOBAL",
                    &self.codes[offset + 1],
                    &self.constants[self.codes[offset + 1] as usize]
                );
                2
            }
            Some(Opcode::DefineGlobalU16) => {
                let high_byte = self.codes[offset + 1] as u16;
                let low_byte = self.codes[offset + 2] as u16;
                let constant_index = (high_byte << 8) | low_byte;
                println!(
                    "{:16} {:4} {}",
                    "OP_DEFINE_GLOBAL_U16",
                    constant_index,
                    &self.constants[constant_index as usize]
                );
                3
            }
            Some(Opcode::GetGlobal) => {
                println!(
                    "{:16} {:4} {}",
                    "OP_GET_GLOBAL",
                    &self.codes[offset + 1],
                    &self.constants[self.codes[offset + 1] as usize]
                );
                2
            }
            Some(Opcode::GetGlobalU16) => {
                let high_byte = self.codes[offset + 1] as u16;
                let low_byte = self.codes[offset + 2] as u16;
                let constant_index = (high_byte << 8) | low_byte;
                println!(
                    "{:16} {:4} {}",
                    "OP_GET_GLOBAL_U16", constant_index, &self.constants[constant_index as usize]
                );
                3
            }
            Some(Opcode::SetGlobal) => {
                println!(
                    "{:16} {:4} {}",
                    "OP_SET_GLOBAL",
                    &self.codes[offset + 1],
                    &self.constants[self.codes[offset + 1] as usize]
                );
                2
            }
            Some(Opcode::SetGlobalU16) => {
                let high_byte = self.codes[offset + 1] as u16;
                let low_byte = self.codes[offset + 2] as u16;
                let constant_index = (high_byte << 8) | low_byte;
                println!(
                    "{:16} {:4} {}",
                    "OP_SET_GLOBAL_U16", constant_index, &self.constants[constant_index as usize]
                );
                3
            }
            Some(Opcode::Print) => {
                println!("{:16}", "OP_PRINT");
                1
            }
            Some(Opcode::Return) => {
                println!("{:16}", "OP_RETURN");
                1
            }
            Some(Opcode::Pop) => {
                println!("{:16}", "OP_POP");
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
            Some(Opcode::True) => {
                println!("{:16}", "OP_TRUE");
                1
            }
            Some(Opcode::False) => {
                println!("{:16}", "OP_FALSE");
                1
            }
            Some(Opcode::Nil) => {
                println!("{:16}", "OP_NIL");
                1
            }
            Some(Opcode::Less) => {
                println!("{:16}", "OP_LESS");
                1
            }
            Some(Opcode::Equal) => {
                println!("{:16}", "OP_EQUAL");
                1
            }
            Some(Opcode::Greater) => {
                println!("{:16}", "OP_GREATER");
                1
            }
            None => {
                println!("{:16}", "Unknown opcode");
                1
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Object(object) => write!(f, "{}", unsafe { object.as_ref() }),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ObjectKind::String(s) => write!(f, "\"{}\"", unsafe { s.as_ref() }),
        }
    }
}
