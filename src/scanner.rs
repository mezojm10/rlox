pub struct Scanner {
    start: usize,
    current: usize,
    line: usize,
    source: String,
}

pub struct Token {
    pub tok_type: TokenType,
    pub line: usize,
    start: usize,
}

impl Token {
    pub fn print_info(&self) {
        println!("{:-10?} {}", &self.tok_type, &self.tok_type.to_string());
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    Identifier(String),
    String(String),
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
    Error(String),
    Eof,
}

impl TokenType {
    fn to_string(&self) -> String {
        match self {
            TokenType::Identifier(s) => s.to_string(),
            TokenType::String(s) => s.to_string(),
            TokenType::Number(n) => n.to_string(),
            TokenType::Error(s) => s.to_string(),
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::LeftBrace => "{".to_string(),
            TokenType::RightBrace => "}".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Slash => "/".to_string(),
            TokenType::Star => "*".to_string(),
            TokenType::Bang => "!".to_string(),
            TokenType::BangEqual => "!=".to_string(),
            TokenType::Equal => "=".to_string(),
            TokenType::EqualEqual => "==".to_string(),
            TokenType::Greater => ">".to_string(),
            TokenType::GreaterEqual => ">=".to_string(),
            TokenType::Less => "<".to_string(),
            TokenType::LessEqual => "<=".to_string(),
            TokenType::And => "and".to_string(),
            TokenType::Class => "class".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::False => "false".to_string(),
            TokenType::For => "for".to_string(),
            TokenType::Fun => "fun".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Nil => "nil".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Print => "print".to_string(),
            TokenType::Return => "return".to_string(),
            TokenType::Super => "super".to_string(),
            TokenType::This => "this".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::Var => "var".to_string(),
            TokenType::While => "while".to_string(),
            TokenType::Eof => "eof".to_string(),
        }
    }
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            start: 0,
            current: 0,
            line: 1,
            source: source.to_string(),
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if self.current >= self.source.len() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if c.is_ascii_digit() {
            return self.scan_number();
        } else if c.is_ascii_alphabetic() || c == '_' {
            // Handle identifiers and keywords
            return self.scan_identifier();
        }

        match c {
            // Single-character tokens
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            ';' => self.make_token(TokenType::Semicolon),
            '*' => self.make_token(TokenType::Star),
            '/' => self.make_token(TokenType::Slash),
            // One or two character tokens
            '!' => {
                if self.expect('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.expect('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.expect('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.expect('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            // Literals and identifiers
            '"' => self.scan_string(),
            // Unhandled characters
            _ => self.make_token(TokenType::Error("Unexpected character".to_string())),
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.current >= self.source.len() {
                break;
            }
            let c = self.source.chars().nth(self.current).unwrap();
            match c {
                // Skip spaces, carriage returns, and tabs
                ' ' | '\r' | '\t' => {
                    self.current += 1;
                }
                // Skip new lines
                '\n' => {
                    self.line += 1;
                    self.current += 1;
                }
                // Skip comments (// to end of line)
                '/' => {
                    if self.current + 1 < self.source.len()
                        && self.source.chars().nth(self.current + 1).unwrap() == '/'
                    {
                        while self.current < self.source.len()
                            && self.source.chars().nth(self.current).unwrap() != '\n'
                        {
                            self.current += 1;
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn scan_string(&mut self) -> Token {
        while self.current < self.source.len() {
            let c = self.source.chars().nth(self.current).unwrap();
            if c == '"' {
                // Closing quote found
                self.current += 1; // Consume closing quote
                let value = self
                    .source
                    .chars()
                    .skip(self.start + 1)
                    .take(self.current - self.start - 2)
                    .collect::<String>();
                return self.make_token(TokenType::String(value));
            } else if c == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }
        // Unterminated string
        self.make_token(TokenType::Error("Unterminated string.".to_string()))
    }

    fn scan_number(&mut self) -> Token {
        while self.current < self.source.len() {
            let c = self.source.chars().nth(self.current).unwrap();
            if !c.is_ascii_digit() {
                break;
            }
            self.current += 1;
        }

        // Look for a fractional part.
        if self.current < self.source.len() && self.source.chars().nth(self.current).unwrap() == '.'
        {
            if self.current + 1 < self.source.len()
                && self
                    .source
                    .chars()
                    .nth(self.current + 1)
                    .unwrap()
                    .is_ascii_digit()
            {
                // Consume the "."
                self.current += 1;

                while self.current < self.source.len() {
                    let c = self.source.chars().nth(self.current).unwrap();
                    if !c.is_ascii_digit() {
                        break;
                    }
                    self.current += 1;
                }
            }
        }

        let number_str: String = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();
        let number_value: f64 = number_str.parse().unwrap();

        self.make_token(TokenType::Number(number_value))
    }

    fn scan_identifier(&mut self) -> Token {
        while self.current < self.source.len() {
            let c = self.source.chars().nth(self.current).unwrap();
            if !c.is_ascii_alphanumeric() && c != '_' {
                break;
            }
            self.current += 1;
        }

        let identifier: String = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();

        let tok_type = match identifier.as_str() {
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
            _ => TokenType::Identifier(identifier),
        };

        self.make_token(tok_type)
    }

    fn advance(&mut self) -> char {
        let c = self
            .source
            .chars()
            .nth(self.current)
            .expect("No character at current position");
        self.current += 1;
        c
    }

    fn expect(&mut self, expected: char) -> bool {
        if self.current >= self.source.len() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn make_token(&self, tok_type: TokenType) -> Token {
        Token {
            tok_type,
            line: self.line,
            start: self.start,
        }
    }
}
