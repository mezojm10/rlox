use std::fmt::{self, Display};

use miette::{LabeledSpan, Result};

pub struct Lexer<'de> {
    source: &'de str,
    rest: &'de str,
    current: usize,
    start: usize,
    line: usize,
    peeked: Option<Result<Token<'de>>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub kind: TokenType,
    pub line: usize,
    pub offset: usize,
    pub origin: &'de str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number(f64),
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl<'de> Display for Token<'de> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let origin = self.origin;
        write!(
            f,
            "{}",
            match self.kind {
                TokenType::Identifier => format!("IDENTIFIER {origin}"),
                TokenType::String => format!("STRING \"{origin}\""),
                TokenType::Number(n) => format!("NUMBER {n}"),
                TokenType::LeftParen => format!("LEFT_PAREN {origin}"),
                TokenType::RightParen => format!("RIGHT_PAREN {origin}"),
                TokenType::LeftBrace => format!("LEFT_BRACKET {origin}"),
                TokenType::RightBrace => format!("RIGHT_BRACKET {origin}"),
                TokenType::Comma => format!("COMMA {origin}"),
                TokenType::Dot => format!("DOT {origin}"),
                TokenType::Minus => format!("MINUS {origin}"),
                TokenType::Plus => format!("PLUS {origin}"),
                TokenType::Semicolon => format!("SEMICOLON {origin}"),
                TokenType::Slash => format!("SLASH {origin}"),
                TokenType::Star => format!("STAR {origin}"),
                TokenType::Bang => format!("BANG {origin}"),
                TokenType::BangEqual => format!("BANG_EQUAL {origin}"),
                TokenType::Equal => format!("EQUAL {origin}"),
                TokenType::EqualEqual => format!("EQUAL_EQUAL {origin}"),
                TokenType::Greater => format!("GREATER {origin}"),
                TokenType::GreaterEqual => format!("GREATER_EQUAL {origin}"),
                TokenType::Less => format!("LESS {origin}"),
                TokenType::LessEqual => format!("LESS_EQUAL {origin}"),
                TokenType::And => format!("AND {origin}"),
                TokenType::Class => format!("CLASS {origin}"),
                TokenType::Else => format!("ELSE {origin}"),
                TokenType::False => format!("FALSE {origin}"),
                TokenType::For => format!("FOR {origin}"),
                TokenType::Fun => format!("FUN {origin}"),
                TokenType::If => format!("IF {origin}"),
                TokenType::Nil => format!("NIL {origin}"),
                TokenType::Or => format!("OR {origin}"),
                TokenType::Print => format!("PRINT {origin}"),
                TokenType::Return => format!("RETURN {origin}"),
                TokenType::Super => format!("SUPER {origin}"),
                TokenType::This => format!("THIS {origin}"),
                TokenType::True => format!("TRUE {origin}"),
                TokenType::Var => format!("VAR {origin}"),
                TokenType::While => format!("WHILE {origin}"),
            }
        )
    }
}

impl<'de> Lexer<'de> {
    pub fn new(source: &'de str) -> Self {
        Lexer {
            current: 0,
            start: 0,
            source: source,
            rest: source,
            line: 1,
            peeked: None,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            // This needs to be inside the loop to get updated `self.rest`
            let mut chars = self.rest.chars();
            let c = match chars.next() {
                Some(c) => c,
                None => break,
            };

            match c {
                // Skip spaces, carriage returns, and tabs
                ' ' | '\r' | '\t' => {
                    self.rest = chars.as_str();
                    self.current += 1;
                }
                // Skip new lines
                '\n' => {
                    self.rest = chars.as_str();
                    self.line += 1;
                    self.current += 1;
                }
                // Skip comments (// to end of line)
                '/' => {
                    if self.rest[1..].starts_with('/') {
                        // It's a comment, skip to end of line
                        let newline = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.current += newline;
                        self.rest = &self.rest[newline..];
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn scan_string(&mut self) -> Result<Token<'de>> {
        let mut chars = self.rest.chars();
        loop {
            let c = match chars.next() {
                Some(c) => c,
                None => break,
            };
            if c == '"' {
                // Closing quote found
                self.current += 1; // Consume closing quote
                self.rest = chars.as_str();
                let start = &self.source[self.start + 1..self.current - 1];
                return Ok(self.make_token(TokenType::String, start));
            } else if c == '\n' {
                self.line += 1;
            }
            self.current += c.len_utf8();
        }
        // Unterminated string
        self.rest = chars.as_str();
        Err(miette::miette! {
            labels = vec![
                LabeledSpan::at(self.start..self.start + 1, "the string literal starts here")
            ],
            "Unterminated string literal"
        }
        .with_source_code(self.source.to_string()))
    }

    fn scan_number(&mut self) -> Result<Token<'de>> {
        self.scan_digits(self.rest);

        // Look for a fractional part
        if self.rest.starts_with('.') {
            let count = self.scan_digits(&self.rest[1..]);
            if count != 0 {
                self.current += 1;
            }
        }

        let number_str = &self.source[self.start..self.current];

        match number_str.parse() {
            Ok(n) => Ok(self.make_token(TokenType::Number(n), number_str)),
            Err(_) => Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(self.start..self.current, "the number literal")
                ],
                "Invalid number literal '{}'", number_str
            }
            .with_source_code(self.source.to_string())),
        }
    }

    fn scan_digits(&mut self, num: &'de str) -> usize {
        let first_non_digit = num.find(|c| !matches!(c, '0'..='9')).unwrap_or(num.len());

        self.rest = &num[first_non_digit..];
        self.current += first_non_digit;
        first_non_digit
    }

    fn scan_identifier(&mut self) -> Token<'de> {
        let first_non_ident_char = self
            .rest
            .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
            .unwrap_or(self.rest.len());

        self.rest = &self.rest[first_non_ident_char..];
        self.current += first_non_ident_char;

        let identifier = &self.source[self.start..self.current];

        let tok_type = match identifier {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };

        self.make_token(tok_type, &identifier)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.rest.chars().next()?;
        self.rest = &self.rest[c.len_utf8()..];
        self.current += 1;
        Some(c)
    }

    pub fn expect(&mut self, expected: TokenType, unexpected: &str) -> Result<Token<'de>> {
        self.expect_where(|t| t.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'de>> {
        match self.next() {
            Some(Ok(tok)) if check(&tok) => Ok(tok),
            Some(Ok(token)) => Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")
                ],
                help = format!("Expected {token:?}"),
                "{unexpected}"
            }
            .with_source_code(self.source.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(miette::miette!("Unexpected EOF")),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }

    fn make_token(&self, tok_type: TokenType, start: &'de str) -> Token<'de> {
        Token {
            kind: tok_type,
            line: self.line,
            offset: self.start,
            origin: start,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>>;

    fn next(&mut self) -> Option<Self::Item> {
        {
            if self.peeked.is_some() {
                return self.peeked.take();
            }

            self.skip_whitespace();

            self.start = self.current;

            let c = self.advance()?;

            // Determine what kind of token we're starting
            enum Started {
                String,
                Number,
                Ident,
                IfEqualElse(TokenType, TokenType),
            }

            let c_str = &self.source[self.start..self.current];

            let started = match c {
                // Single-character tokens
                '(' => return Some(Ok(self.make_token(TokenType::LeftParen, c_str))),
                ')' => return Some(Ok(self.make_token(TokenType::RightParen, c_str))),
                '{' => return Some(Ok(self.make_token(TokenType::LeftBrace, c_str))),
                '}' => return Some(Ok(self.make_token(TokenType::RightBrace, c_str))),
                ',' => return Some(Ok(self.make_token(TokenType::Comma, c_str))),
                '.' => return Some(Ok(self.make_token(TokenType::Dot, c_str))),
                '-' => return Some(Ok(self.make_token(TokenType::Minus, c_str))),
                '+' => return Some(Ok(self.make_token(TokenType::Plus, c_str))),
                ';' => return Some(Ok(self.make_token(TokenType::Semicolon, c_str))),
                '*' => return Some(Ok(self.make_token(TokenType::Star, c_str))),
                '/' => return Some(Ok(self.make_token(TokenType::Slash, c_str))),

                // One or two character tokens
                '!' => Started::IfEqualElse(TokenType::BangEqual, TokenType::Bang),
                '=' => Started::IfEqualElse(TokenType::EqualEqual, TokenType::Equal),
                '<' => Started::IfEqualElse(TokenType::LessEqual, TokenType::Less),
                '>' => Started::IfEqualElse(TokenType::GreaterEqual, TokenType::Greater),

                // Literals and identifiers
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,

                // Unhandled characters
                _ => {
                    return Some(Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(self.current..self.current + c.len_utf8(), "this character")
                        ],
                        "Unexpected token '{c}' in input"
                    }
                    .with_source_code(self.source.to_string())));
                }
            };

            match started {
                Started::String => Some(self.scan_string()),
                Started::Number => Some(self.scan_number()),
                Started::Ident => Some(Ok(self.scan_identifier())),
                Started::IfEqualElse(yes, no) => {
                    let token = if self.rest.starts_with('=') {
                        self.current += 1;
                        self.rest = &self.rest[1..];
                        self.make_token(yes, &self.source[self.start..self.current])
                    } else {
                        self.make_token(no, c_str)
                    };
                    Some(Ok(token))
                }
            }
        }
    }
}
