use miette::{Context, LabeledSpan};

use crate::lex::{Lexer, Token, TokenType};
use crate::vm::{self, Opcode, Value};

pub struct Parser<'de> {
    source: &'de str,
    lexer: Lexer<'de>,
    pub bytecode_chunk: vm::Chunk,
}

impl<'de> Parser<'de> {
    pub fn new(source: &'de str) -> Self {
        Parser {
            source,
            lexer: Lexer::new(source),
            bytecode_chunk: vm::Chunk::new(),
        }
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
            TokenType::Number(n) /* | TokenType::Identifier */ => {
                // Handle literals and identifiers
                self.bytecode_chunk.emit_constant(Value::Number(n), lhs.line)?;
            }
            TokenType::True => {
                self.bytecode_chunk.emit_constant(Value::Bool(true), lhs.line)?;
            }
            TokenType::False => {
                self.bytecode_chunk.emit_constant(Value::Bool(false), lhs.line)?;
            }
            TokenType::Nil => {
                self.bytecode_chunk.emit_constant(Value::Nil, lhs.line)?;
            }

            // Groups
            TokenType::LeftParen => {
                self.expr_within(0).wrap_err("inside bracketed expression")?;
                self.lexer
                    .expect(TokenType::RightParen, "Unexpected end of bracketed expression")
                    .wrap_err("after bracketed expression")?;
            }

            // Unary prefix expressions
            TokenType::Minus => {
                let ((), r_bp) = prefix_binding_power(Op::Minus);
                self.expr_within(r_bp).wrap_err("after unary minus")?;
                // Emit bytecode for unary minus
                self.bytecode_chunk.emit_op(Opcode::Negate, lhs.line);
            }
            TokenType::Bang => {
                let ((), r_bp) = prefix_binding_power(Op::Bang);
                self.expr_within(r_bp).wrap_err("after unary bang")?;
                // Emit bytecode for unary bang
                self.bytecode_chunk.emit_op(Opcode::Not, lhs.line);
            }

            token => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(lhs.offset..lhs.origin.len(),"here"),
                    ],
                    help = format!("Unexpected {token:?}"),
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
                        help = format!("Unexpected {token:?}"),
                        "Expected an infix operator"
                    }
                    .with_source_code(self.source.to_string()))
                }
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                self.expr_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {op}"))?;

                match op {
                    Op::Plus => self.bytecode_chunk.emit_op(Opcode::Add, lhs.line),
                    Op::Minus => self.bytecode_chunk.emit_op(Opcode::Subtract, lhs.line),
                    Op::Star => self.bytecode_chunk.emit_op(Opcode::Multiply, lhs.line),
                    Op::Slash => self.bytecode_chunk.emit_op(Opcode::Divide, lhs.line),
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
