use miette::{Context, LabeledSpan};

use crate::lex::{Lexer, Token, TokenType};
use crate::vm::{self, Opcode, Value};

pub struct Parser<'de> {
    source: &'de str,
    lexer: Lexer<'de>,
    pub vm: vm::VM,
}

impl<'de> Parser<'de> {
    pub fn new(source: &'de str) -> Self {
        Parser {
            source,
            lexer: Lexer::new(source),
            vm: vm::VM::new(),
        }
    }

    pub fn parse(mut self) -> Result<vm::VM, miette::Error> {
        while self.lexer.peek().is_some() {
            self.stmt_within(0).wrap_err("in top-level statement")?;
        }
        Ok(self.vm)
    }

    fn stmt_within(&mut self, _min_bp: u8) -> Result<(), miette::Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on left-hand side"),
            None => return Ok(()),
        };

        match lhs.kind {
            TokenType::Print => {
                self.expr().wrap_err("in print statement")?;
                self.lexer
                    .expect(TokenType::Semicolon, "Unexpected end of print statement")
                    .wrap_err("after print statement")?;
                self.vm.chunk.emit_op(Opcode::Print, lhs.line);
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
                    self.expr().wrap_err("in variable initializer")?;
                } else {
                    // Default initialize to nil
                    self.vm.chunk.emit_op(Opcode::Nil, lhs.line);
                }

                self.lexer
                    .expect(
                        TokenType::Semicolon,
                        "Unexpected end of variable declaration",
                    )
                    .wrap_err("after variable declaration")?;

                self.vm.emit_define_global(var_name, lhs.line)?;
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
                    self.expr().wrap_err("in variable assignment")?;

                    // Consume semicolon
                    self.lexer
                        .expect(
                            TokenType::Semicolon,
                            "Unexpected end of variable assignment",
                        )
                        .wrap_err("after variable assignment")?;

                    // Emit set global
                    self.vm.emit_set_global(lhs.origin, lhs.line)?;
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

    pub fn expr(&mut self) -> Result<(), miette::Error> {
        self.expr_within(0)
    }

    fn expr_within(&mut self, min_bp: u8) -> Result<(), miette::Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on left-hand side"),
            None => return Ok(()),
        };

        match lhs.kind {
            // Atoms
            TokenType::Number(n) => {
                self.vm.chunk.emit_constant(Value::Number(n), lhs.line)?;
            }
            TokenType::Identifier => {
                self.vm.emit_get_global(lhs.origin, lhs.line)?;
            }
            TokenType::String => {
                self.vm.emit_string(lhs.origin, lhs.line)?;
            }
            TokenType::True => {
                self.vm.chunk.emit_op(Opcode::True, lhs.line);
            }
            TokenType::False => {
                self.vm.chunk.emit_op(Opcode::False, lhs.line);
            }
            TokenType::Nil => {
                self.vm.chunk.emit_op(Opcode::Nil, lhs.line);
            }

            // Groups
            TokenType::LeftParen => {
                self.expr_within(0)
                    .wrap_err("inside bracketed expression")?;
                self.lexer
                    .expect(
                        TokenType::RightParen,
                        "Unexpected end of bracketed expression",
                    )
                    .wrap_err("after bracketed expression")?;
            }

            // Unary prefix expressions
            TokenType::Minus => {
                let ((), r_bp) = prefix_binding_power(Op::Minus);
                self.expr_within(r_bp).wrap_err("after unary minus")?;
                // Emit bytecode for unary minus
                self.vm.chunk.emit_op(Opcode::Negate, lhs.line);
            }
            TokenType::Bang => {
                let ((), r_bp) = prefix_binding_power(Op::Bang);
                self.expr_within(r_bp).wrap_err("after unary bang")?;
                // Emit bytecode for unary bang
                self.vm.chunk.emit_op(Opcode::Not, lhs.line);
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

                self.expr_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {op}"))?;

                match op {
                    Op::Plus => self.vm.chunk.emit_op(Opcode::Add, lhs.line),
                    Op::Minus => self.vm.chunk.emit_op(Opcode::Subtract, lhs.line),
                    Op::Star => self.vm.chunk.emit_op(Opcode::Multiply, lhs.line),
                    Op::Slash => self.vm.chunk.emit_op(Opcode::Divide, lhs.line),
                    Op::EqualEqual => self.vm.chunk.emit_op(Opcode::Equal, lhs.line),
                    Op::BangEqual => {
                        self.vm.chunk.emit_op(Opcode::Equal, lhs.line);
                        self.vm.chunk.emit_op(Opcode::Not, lhs.line);
                    }
                    Op::Less => self.vm.chunk.emit_op(Opcode::Less, lhs.line),
                    Op::LessEqual => {
                        self.vm.chunk.emit_op(Opcode::Greater, lhs.line);
                        self.vm.chunk.emit_op(Opcode::Not, lhs.line);
                    }
                    Op::Greater => self.vm.chunk.emit_op(Opcode::Greater, lhs.line),
                    Op::GreaterEqual => {
                        self.vm.chunk.emit_op(Opcode::Less, lhs.line);
                        self.vm.chunk.emit_op(Opcode::Not, lhs.line);
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
