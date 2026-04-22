use miette::{Context, LabeledSpan};

use crate::lex::{Lexer, Token, TokenType};
use crate::vm::{self, Opcode, Value};

pub struct Parser<'de> {
    source: &'de str,
    lexer: Lexer<'de>,
    compiler: Compiler<'de>,
}

pub struct Compiler<'de> {
    scope_depth: usize,
    locals: Vec<Local<'de>>,
}

impl<'de> Compiler<'de> {
    fn add_local(&mut self, name: Token<'de>, src: &str) -> Result<(), miette::Error> {
        // Max amount of locals allowed is 256 to use 1 byte as an index
        // TODO: Make second instruction that uses 2 bytes?
        if self.locals.len() >= 256 {
            return Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(name.offset..name.origin.len(), "here"),
                ],
                help = "Too many local variables in scope",
                "Maximum is 256"
            }
            .with_source_code(src.to_string()));
        }

        // Check if redeclaring in the same scope
        for local in self.locals.iter().rev() {
            if
            /*local.depth != -1 && */
            local.depth < self.scope_depth {
                // Doesn't matter if declared in a previous scope (shadowing is allowed)
                break;
            }

            if name.origin == local.name_token.origin {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(name.offset..name.origin.len(), "here"),
                    ],
                    help = "Cannot redeclare variable in the same scope",
                    "Variable already declared"
                });
            }
        }

        self.locals.push(Local {
            name_token: name,
            depth: self.scope_depth,
        });

        Ok(())
    }

    fn resolve_local(&self, name: Token<'de>) -> Option<u8> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if dbg!(local.name_token.origin) == name.origin {
                return Some(i as u8);
            }
        }

        None
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Local<'de> {
    name_token: Token<'de>,
    depth: usize,
}

impl<'de> Parser<'de> {
    pub fn new(source: &'de str) -> Self {
        Parser {
            source,
            lexer: Lexer::new(source),
            compiler: Compiler {
                scope_depth: 0,
                locals: Vec::with_capacity(256),
            },
        }
    }

    pub fn parse(mut self, vm: &mut vm::VM) -> Result<(), miette::Error> {
        while self.lexer.peek().is_some() {
            self.stmt_within(vm, 0).wrap_err("in top-level statement")?;
        }
        Ok(())
    }

    fn parse_block(&mut self, vm: &mut vm::VM) -> Result<(), miette::Error> {
        loop {
            let tok = self.lexer.peek();
            if tok.map_or(false, |tok| tok.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in block statement");
            }
            match tok.map(|res| res.as_ref().expect("handled Err above")) {
                Some(Token {
                    kind: TokenType::RightBrace,
                    ..
                })
                | None => break,
                _ => self.stmt_within(vm, 0)?,
            };
        }

        self.lexer
            .expect(TokenType::RightBrace, "expected }")
            .wrap_err("at the end of the block")?;

        Ok(())
    }

    fn stmt_within(&mut self, vm: &mut vm::VM, _min_bp: u8) -> Result<(), miette::Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on left-hand side"),
            None => return Ok(()),
        };

        match lhs.kind {
            TokenType::Print => {
                self.expr(vm).wrap_err("in print statement")?;
                self.lexer
                    .expect(TokenType::Semicolon, "expected ;")
                    .wrap_err("after print statement")?;
                vm.chunk.emit_op(Opcode::Print, lhs.line);
                return Ok(());
            }

            TokenType::Var => {
                let name_token = self
                    .lexer
                    .expect(TokenType::Identifier, "Expected variable name after 'var'")
                    .wrap_err("after 'var'")?;

                let var_name = name_token.origin;

                // Check for initializer
                if self.lexer.peek().map_or(false, |t| {
                    t.as_ref().map_or(false, |tok| tok.kind == TokenType::Equal)
                }) {
                    self.lexer.next();
                    self.expr(vm).wrap_err("in variable initializer")?;
                } else {
                    // Default initialize to nil
                    vm.chunk.emit_op(Opcode::Nil, lhs.line);
                }

                self.lexer
                    .expect(TokenType::Semicolon, "expected ;")
                    .wrap_err("after variable declaration")?;

                if self.compiler.scope_depth > 0 {
                    // Add local variable
                    self.compiler.add_local(name_token, self.source)?;
                } else {
                    // Add global variable
                    vm.emit_define_global(var_name, lhs.line)?;
                }
            }

            // Set variable
            TokenType::Identifier => {
                // We are setting a value if there's an equal token after the ident
                if self.lexer.peek().map_or(false, |t| {
                    t.as_ref().map_or(false, |tok| tok.kind == TokenType::Equal)
                }) {
                    // Skip assignment token
                    self.lexer.next();

                    // Parse expression
                    self.expr(vm).wrap_err("in variable assignment")?;

                    // Consume semicolon
                    self.lexer
                        .expect(TokenType::Semicolon, "expected ;")
                        .wrap_err("after variable assignment")?;

                    if let Some(local_idx) = self.compiler.resolve_local(lhs) {
                        // Emit set local
                        vm.emit_set_local(local_idx, lhs.line);
                    } else {
                        // Emit set global
                        vm.emit_set_global(lhs.origin, lhs.line)?;
                    }
                } else {
                    match self.lexer.next() {
                        Some(Ok(tok)) => {
                            return Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(tok.offset..tok.origin.len(),"here"),
                                ],
                                help = format!("Unexpected {tok} after identifier {lhs}"),
                                "Expected a statement"
                            }
                            .with_source_code(self.source.to_string()))
                        }
                        None => {
                            return Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(lhs.offset..lhs.origin.len(),"here"),
                                ],
                                help = format!("Unexpected end of input after identifier {lhs}"),
                                "Expected a statement"
                            }
                            .with_source_code(self.source.to_string()));
                        }
                        Some(Err(e)) => return Err(e).wrap_err("after identifier"),
                    };
                }
            }

            // Block statements
            TokenType::LeftBrace => {
                // Begin new scope
                self.compiler.scope_depth += 1;

                self.parse_block(vm)?;

                // End scope
                self.compiler.scope_depth -= 1;

                // Remove locals that belong to the closed scope
                while self.compiler.locals.len() > 0 {
                    let local = match self.compiler.locals.last() {
                        Some(local) => local,
                        None => {
                            return Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(lhs.offset..lhs.origin.len(), "here"),
                                ],
                                help = format!("This is a bug in the interpreter"),
                                "Something went wrong",
                            }
                            .with_source_code(self.source.to_string()))
                        }
                    };

                    if local.depth <= self.compiler.scope_depth {
                        break;
                    }

                    vm.chunk.emit_op(Opcode::Pop, local.name_token.line);
                    self.compiler.locals.pop();
                }
            }

            // If statements
            TokenType::If => {
                // Consume '('
                self.lexer
                    .expect(TokenType::LeftParen, "expected (")
                    .wrap_err("after if")?;

                // Parse condition
                self.expr(vm).wrap_err("in if condition")?;

                // consume ')'
                self.lexer
                    .expect(TokenType::RightParen, "expected )")
                    .wrap_err("after if condition")?;

                let then_jump = vm.chunk.emit_jump(Opcode::JumpIfFalse, lhs.line);
                vm.chunk.emit_op(Opcode::Pop, lhs.line);
                self.stmt_within(vm, 0)?;

                let else_jump = vm.chunk.emit_jump(Opcode::Jump, lhs.line);

                vm.chunk.patch_jump(then_jump);
                vm.chunk.emit_op(Opcode::Pop, lhs.line);

                if self.lexer.peek().map_or(false, |t| {
                    t.as_ref().map_or(false, |tok| tok.kind == TokenType::Else)
                }) {
                    self.lexer.next();
                    self.stmt_within(vm, 0).wrap_err("in else statement")?;
                }

                vm.chunk.patch_jump(else_jump);
            }

            _ => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(lhs.offset..lhs.origin.len(),"here"),
                    ],
                    help = format!("Unexpected {lhs}"),
                    "Expected a statement"
                }
                .with_source_code(self.source.to_string()));
            }
        }

        Ok(())
    }

    pub fn expr(&mut self, vm: &mut vm::VM) -> Result<(), miette::Error> {
        self.expr_within(vm, 0)
    }

    fn expr_within(&mut self, vm: &mut vm::VM, min_bp: u8) -> Result<(), miette::Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on left-hand side"),
            None => return Ok(()),
        };

        match lhs.kind {
            // Atoms
            TokenType::Number(n) => {
                vm.chunk.emit_constant(Value::Number(n), lhs.line)?;
            }
            TokenType::Identifier => {
                if let Some(local_idx) = dbg!(self.compiler.resolve_local(lhs)) {
                    // Emit get local
                    vm.emit_get_local(local_idx, lhs.line);
                } else {
                    // Emit get global
                    vm.emit_get_global(lhs.origin, lhs.line)?;
                }
            }
            TokenType::String => {
                vm.emit_string(lhs.origin, lhs.line)?;
            }
            TokenType::True => {
                vm.chunk.emit_op(Opcode::True, lhs.line);
            }
            TokenType::False => {
                vm.chunk.emit_op(Opcode::False, lhs.line);
            }
            TokenType::Nil => {
                vm.chunk.emit_op(Opcode::Nil, lhs.line);
            }

            // Groups
            TokenType::LeftParen => {
                self.expr_within(vm, 0)
                    .wrap_err("inside bracketed expression")?;
                self.lexer
                    .expect(TokenType::RightParen, "expected )")
                    .wrap_err("after bracketed expression")?;
            }

            // Unary prefix expressions
            TokenType::Minus => {
                let ((), r_bp) = prefix_binding_power(Op::Minus);
                self.expr_within(vm, r_bp).wrap_err("after unary minus")?;
                // Emit bytecode for unary minus
                vm.chunk.emit_op(Opcode::Negate, lhs.line);
            }
            TokenType::Bang => {
                let ((), r_bp) = prefix_binding_power(Op::Bang);
                self.expr_within(vm, r_bp).wrap_err("after unary bang")?;
                // Emit bytecode for unary bang
                vm.chunk.emit_op(Opcode::Not, lhs.line);
            }

            _ => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(lhs.offset..lhs.origin.len(),"here"),
                    ],
                    help = format!("Unexpected {lhs}"),
                    "Expected an expression"
                }
                .with_source_code(self.source.to_string()));
            }
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in place of expected operator");
            }
            let op = match op.map(|res| res.as_ref().expect("handled Err above")) {
                // End of expression
                None => break,
                Some(Token {
                    kind:
                        TokenType::RightParen
                        | TokenType::RightBrace
                        | TokenType::Comma
                        | TokenType::Semicolon,
                    ..
                }) => break,

                // Valid operators
                Some(Token {
                    kind: TokenType::Minus,
                    ..
                }) => Op::Minus,
                Some(Token {
                    kind: TokenType::Plus,
                    ..
                }) => Op::Plus,
                Some(Token {
                    kind: TokenType::Star,
                    ..
                }) => Op::Star,
                Some(Token {
                    kind: TokenType::Slash,
                    ..
                }) => Op::Slash,
                Some(Token {
                    kind: TokenType::EqualEqual,
                    ..
                }) => Op::EqualEqual,
                Some(Token {
                    kind: TokenType::BangEqual,
                    ..
                }) => Op::BangEqual,
                Some(Token {
                    kind: TokenType::Less,
                    ..
                }) => Op::Less,
                Some(Token {
                    kind: TokenType::LessEqual,
                    ..
                }) => Op::LessEqual,
                Some(Token {
                    kind: TokenType::Greater,
                    ..
                }) => Op::Greater,
                Some(Token {
                    kind: TokenType::GreaterEqual,
                    ..
                }) => Op::GreaterEqual,
                Some(Token {
                    kind: TokenType::And,
                    ..
                }) => Op::And,
                Some(Token {
                    kind: TokenType::Or,
                    ..
                }) => Op::Or,
                Some(Token {
                    kind: TokenType::Dot,
                    ..
                }) => Op::Field,
                Some(Token {
                    kind: TokenType::LeftParen,
                    ..
                }) => Op::Call,

                Some(token) => {
                    return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(token.offset..token.origin.len(), "here"),
                        ],
                        help = format!("Unexpected {token}"),
                        "Expected an infix or postfix operator"
                    }
                    .with_source_code(self.source.to_string()))
                }
            };

            // Postfix operators
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                continue;
            }

            // Infix operators
            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                self.expr_within(vm, r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {op}"))?;

                match op {
                    Op::Plus => vm.chunk.emit_op(Opcode::Add, lhs.line),
                    Op::Minus => vm.chunk.emit_op(Opcode::Subtract, lhs.line),
                    Op::Star => vm.chunk.emit_op(Opcode::Multiply, lhs.line),
                    Op::Slash => vm.chunk.emit_op(Opcode::Divide, lhs.line),
                    Op::EqualEqual => vm.chunk.emit_op(Opcode::Equal, lhs.line),
                    Op::BangEqual => {
                        vm.chunk.emit_op(Opcode::Equal, lhs.line);
                        vm.chunk.emit_op(Opcode::Not, lhs.line);
                    }
                    Op::Less => vm.chunk.emit_op(Opcode::Less, lhs.line),
                    Op::LessEqual => {
                        vm.chunk.emit_op(Opcode::Greater, lhs.line);
                        vm.chunk.emit_op(Opcode::Not, lhs.line);
                    }
                    Op::Greater => vm.chunk.emit_op(Opcode::Greater, lhs.line),
                    Op::GreaterEqual => {
                        vm.chunk.emit_op(Opcode::Less, lhs.line);
                        vm.chunk.emit_op(Opcode::Not, lhs.line);
                    }
                    _ => {}
                }

                continue;
            }
            break;
        }

        Ok(())
    }
}

// OP BP
// = 1 2
// or 3 4
// and 5 6
// == != 7 8
// < > <= >= 9 10
// + - 11 12
// * / 13 14
// - ! 15
// . () 17

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Or,
    And,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Field,
    Call,
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus | Op::Bang => ((), 15),
        _ => panic!("Unknown prefix operator"),
    }
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    match op {
        Op::Call => Some((17, ())),
        _ => None,
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    match op {
        Op::Or => Some((3, 4)),
        Op::And => Some((5, 6)),
        Op::EqualEqual | Op::BangEqual => Some((7, 8)),
        Op::Less | Op::LessEqual | Op::Greater | Op::GreaterEqual => Some((9, 10)),
        Op::Plus | Op::Minus => Some((11, 12)),
        Op::Star | Op::Slash => Some((13, 14)),
        Op::Field => Some((18, 17)),
        _ => None,
    }
}

use std::fmt;
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::LessEqual => "<=",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::And => "and",
                Op::Or => "or",
                // Op::For => "for",
                // Op::Class => "class",
                // Op::Print => "print",
                // Op::Return => "return",
                Op::Field => ".",
                // Op::Var => "var",
                // Op::While => "while",
                Op::Call => "call",
                // Op::Group => "group",
            }
        )
    }
}
