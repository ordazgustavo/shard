use std::{borrow::Borrow, fmt::Display, iter::Peekable, ops::Range, str::CharIndices};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Unknown,
    Eof,

    Ident(String),
    String(String),
    Integer(String),
    Float(String),

    Let,
    Fn,
    Type,
    Struct,

    RParen,
    LParen,
    RBrace,
    LBrace,
    RBracket,
    LBracket,
    Equal,
    Dot,
    Comma,
    Colon,
    Semicolon,
    ThinArrow,

    Plus,
    Minus,
    EqualTo,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(&format!("{self:?}"))
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::Ident(other)
    }
}

impl<'a> From<&'a str> for TokenKind {
    fn from(other: &'a str) -> TokenKind {
        TokenKind::Ident(other.to_string())
    }
}

#[derive(Debug)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn range(&self) -> Range<usize> {
        self.start..self.end + 1
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Span {
            start: value,
            end: value,
        }
    }
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Span {
            start: value.0,
            end: value.1,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: impl Into<Span>) -> Self {
        Self {
            kind,
            span: span.into(),
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src }
    }

    pub fn iter(&self) -> LexerIter<'a> {
        LexerIter::new(self.src)
    }
}

pub struct LexerIter<'a> {
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    exhausted: bool,
}

impl<'a> LexerIter<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
            exhausted: false,
        }
    }

    fn handle_literal(&mut self, kind: TokenKind, idx: usize) -> Option<Token> {
        Some(Token::new(kind, idx))
    }

    fn handle_dash(&mut self, start: usize) -> Option<Token> {
        if let Some((end, _)) = self.chars.next_if(|(_, ch)| *ch == '>') {
            Some(Token::new(TokenKind::ThinArrow, (start, end)))
        } else {
            Some(Token::new(TokenKind::Minus, start))
        }
    }

    fn handle_equal(&mut self, start: usize) -> Option<Token> {
        if let Some((end, _)) = self.chars.next_if(|(_, ch)| *ch == '=') {
            Some(Token::new(TokenKind::EqualTo, (start, end)))
        } else {
            Some(Token::new(TokenKind::Equal, start))
        }
    }

    fn handle_string(&mut self, start: usize) -> Option<Token> {
        let mut end = start;

        while let Some((idx, ch)) = self.chars.next() {
            end = idx;
            if ch == '"' {
                break;
            }
        }

        let span = Span { start, end };
        let slice = &self.src[span.range()];

        Some(Token::new(TokenKind::String(slice.to_string()), span))
    }

    fn handle_number(&mut self, start: usize) -> Option<Token> {
        let mut end = start;

        let mut is_float = false;
        while let Some((idx, ch)) = self.chars.next_if(|(_, ch)| *ch == '.' || ch.is_digit(10)) {
            end = idx;
            if is_float {
                if let Some((_, ch)) = self.chars.peek() {
                    if *ch == '.' {
                        break;
                    }
                }
            }
            if ch == '.' {
                is_float = true;
            }
        }

        let span = Span { start, end };
        let slice = &self.src[span.range()];

        if is_float {
            Some(Token::new(TokenKind::Float(slice.to_string()), span))
        } else {
            Some(Token::new(TokenKind::Integer(slice.to_string()), span))
        }
    }

    fn handle_ident(&mut self, start: usize) -> Option<Token> {
        let mut end = start;

        while let Some((idx, _)) = self
            .chars
            .next_if(|(_, ch)| *ch == '_' || ch.is_alphanumeric())
        {
            end = idx;
        }

        let span = Span { start, end };
        let slice = &self.src[span.range()];

        match slice.borrow() {
            "let" => Some(Token::new(TokenKind::Let, span)),
            "fn" => Some(Token::new(TokenKind::Fn, span)),
            "type" => Some(Token::new(TokenKind::Type, span)),
            "struct" => Some(Token::new(TokenKind::Struct, span)),
            _ => Some(Token::new(slice.into(), span)),
        }
    }

    fn handle_next_char(&mut self, ch: char, idx: usize) -> Option<Token> {
        match ch {
            '(' => self.handle_literal(TokenKind::RParen, idx),
            ')' => self.handle_literal(TokenKind::LParen, idx),
            '{' => self.handle_literal(TokenKind::RBrace, idx),
            '}' => self.handle_literal(TokenKind::LBrace, idx),
            '[' => self.handle_literal(TokenKind::RBracket, idx),
            ']' => self.handle_literal(TokenKind::LBracket, idx),
            '.' => self.handle_literal(TokenKind::Dot, idx),
            ',' => self.handle_literal(TokenKind::Comma, idx),
            ':' => self.handle_literal(TokenKind::Colon, idx),
            ';' => self.handle_literal(TokenKind::Semicolon, idx),
            '+' => self.handle_literal(TokenKind::Plus, idx),
            '-' => self.handle_dash(idx),
            '=' => self.handle_equal(idx),
            '"' => self.handle_string(idx),
            '0'..='9' => self.handle_number(idx),
            c @ '_' | c if c.is_alphabetic() => self.handle_ident(idx),
            c if c.is_whitespace() => {
                while self.chars.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}
                self.next()
            }
            _ => Some(Token::new(TokenKind::Unknown, idx)),
        }
    }
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some((idx, ch)) => self.handle_next_char(ch, idx),
            None => match self.exhausted {
                true => None,
                false => {
                    self.exhausted = true;
                    Some(Token::new(TokenKind::Eof, 0))
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenKind};
    macro_rules! lexer_test {
        (FAIL: $name:ident, $src:expr) => {
            #[test]
            fn $name() {
                let token = Lexer::new($src)
                    .iter()
                    .find(|t| t.kind == TokenKind::Unknown)
                    .unwrap();

                assert_eq!(
                    token.kind,
                    TokenKind::Unknown,
                    "{:?} should be unknown",
                    token
                );
            }
        };
        ($name:ident, $src:expr => $should_be:expr) => {
            #[test]
            fn $name() {
                let token = Lexer::new($src).iter().next().unwrap();

                assert_eq!(token.kind, $should_be, "Input was {}", $src);
            }
        };
    }

    lexer_test!(FAIL: handles_extraneous_token, "@");
    lexer_test!(FAIL: handles_partially_consumend_input, "let abc @");

    lexer_test!(eof, "" => TokenKind::Eof);
    lexer_test!(eof_spaces_only, " \n \t \n\n \n\r \r \t\t" => TokenKind::Eof);
    lexer_test!(rparen_punctuation, "(" => TokenKind::RParen);
    lexer_test!(lparen_punctuation, ")" => TokenKind::LParen);
    lexer_test!(rbrace_punctuation, "{" => TokenKind::RBrace);
    lexer_test!(lbrace_punctuation, "}" => TokenKind::LBrace);
    lexer_test!(rbracket_punctuation, "[" => TokenKind::RBracket);
    lexer_test!(lbracket_punctuation, "]" => TokenKind::LBracket);
    lexer_test!(equal_punctuation, "=" => TokenKind::Equal);
    lexer_test!(dbl_equal_punctuation, "==" => TokenKind::EqualTo);
    lexer_test!(dot_punctuation, "." => TokenKind::Dot);
    lexer_test!(coma_punctuation, "," => TokenKind::Comma);
    lexer_test!(colon_punctuation, ":" => TokenKind::Colon);
    lexer_test!(semicolon_punctuation, ";" => TokenKind::Semicolon);
    lexer_test!(thin_arrow_punctuation, "->" => TokenKind::ThinArrow);

    lexer_test!(plus_operator, "+" => TokenKind::Plus);
    lexer_test!(minus_operator, "-" => TokenKind::Minus);

    lexer_test!(let_keyword, "let" => TokenKind::Let);
    lexer_test!(fn_keyword, "fn" => TokenKind::Fn);
    lexer_test!(type_keyword, "type" => TokenKind::Type);
    lexer_test!(struct_keyword, "struct" => TokenKind::Struct);

    lexer_test!(identifiers, "abc" => TokenKind::Ident("abc".to_string()));
    lexer_test!(identifiers_single_char, "a" => TokenKind::Ident("a".to_string()));
    lexer_test!(identifiers_single_char_surrounded, " a  " => TokenKind::Ident("a".to_string()));
    lexer_test!(string, "\"abc\"" => TokenKind::String("\"abc\"".to_string()));
    lexer_test!(string_emoji, "\"🫡👨‍👩‍👧‍👦\"" => TokenKind::String("\"🫡👨‍👩‍👧‍👦\"".to_string()));
    lexer_test!(string_with_leading_number, "\"123abc\"" => TokenKind::String("\"123abc\"".to_string()));
    lexer_test!(string_with_special_chars, "\"123\nabc\"" => TokenKind::String("\"123\nabc\"".to_string()));
    lexer_test!(integer, "7" => TokenKind::Integer("7".to_string()));
    lexer_test!(integer_multiple_digits, "42069" => TokenKind::Integer("42069".to_string()));
    lexer_test!(float, "420.69" => TokenKind::Float("420.69".to_string()));
    lexer_test!(float_stops_after_second_dot, "192.168.0" => TokenKind::Float("192.168".to_string()));
}
