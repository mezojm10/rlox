use crate::Lexer;

pub struct Parser<'de> {
    source: &'de str,
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    fn new(source: &'de str) -> Self {
        Parser {
            source,
            lexer: Lexer::new(source),
        }
    }
}
