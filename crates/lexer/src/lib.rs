use std::{borrow::Borrow, fmt::Display, iter::Peekable, ops::RangeInclusive, str::CharIndices};

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
pub struct Token {
    pub kind: TokenKind,
    pub span: RangeInclusive<usize>,
}

impl Token {
    pub fn new(kind: TokenKind, span: RangeInclusive<usize>) -> Self {
        Self { kind, span }
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
    src: Peekable<CharIndices<'a>>,
}

impl<'a> LexerIter<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src: src.char_indices().peekable(),
        }
    }

    fn handle_literal(&mut self, kind: TokenKind, idx: usize) -> Option<Token> {
        Some(Token::new(kind, idx..=idx))
    }

    fn handle_dash(&mut self, start: usize) -> Option<Token> {
        if let Some((end, _)) = self.src.next_if(|(_, ch)| *ch == '>') {
            Some(Token::new(TokenKind::ThinArrow, start..=end))
        } else {
            Some(Token::new(TokenKind::Minus, start..=start))
        }
    }

    fn handle_equal(&mut self, start: usize) -> Option<Token> {
        if let Some((end, _)) = self.src.next_if(|(_, ch)| *ch == '=') {
            Some(Token::new(TokenKind::EqualTo, start..=end))
        } else {
            Some(Token::new(TokenKind::Equal, start..=start))
        }
    }

    fn handle_string(&mut self, start: usize) -> Option<Token> {
        let mut end = start;

        let mut s = String::new();

        while let Some((idx, ch)) = self.src.next() {
            end = idx;
            if ch == '"' {
                break;
            }
            s.push(ch);
        }

        Some(Token::new(TokenKind::String(s), start..=end))
    }

    fn handle_number(&mut self, ch: char, start: usize) -> Option<Token> {
        let mut end = start;

        let mut s = String::new();
        s.push(ch);

        let mut is_float = false;
        while let Some((idx, ch)) = self.src.next_if(|(_, ch)| *ch == '.' || ch.is_digit(10)) {
            end = idx;
            s.push(ch);
            if is_float {
                if let Some((_, ch)) = self.src.peek() {
                    if *ch == '.' {
                        break;
                    }
                }
            }
            if ch == '.' {
                is_float = true;
            }
        }

        if is_float {
            Some(Token::new(TokenKind::Float(s), start..=end))
        } else {
            Some(Token::new(TokenKind::Integer(s), start..=end))
        }
    }

    fn handle_ident(&mut self, ch: char, start: usize) -> Option<Token> {
        let mut end = start;

        let mut id = String::new();
        id.push(ch);

        while let Some((idx, ch)) = self
            .src
            .next_if(|(_, ch)| *ch == '_' || ch.is_alphanumeric())
        {
            id.push(ch);
            end = idx;
        }

        let range = start..=end;

        match id.borrow() {
            "let" => Some(Token::new(TokenKind::Let, range)),
            "fn" => Some(Token::new(TokenKind::Fn, range)),
            "type" => Some(Token::new(TokenKind::Type, range)),
            "struct" => Some(Token::new(TokenKind::Struct, range)),
            _ => Some(Token::new(id.into(), range)),
        }
    }
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some((idx, ch)) = self.src.next() else { return None };

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
            '0'..='9' => self.handle_number(ch, idx),
            c @ '_' | c if c.is_alphabetic() => self.handle_ident(ch, idx),
            c if c.is_whitespace() => {
                while self.src.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}
                self.next()
            }
            _ => Some(Token::new(TokenKind::Unknown, idx..=idx)),
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

    #[test]
    fn handles_empty_input() {
        let token = Lexer::new("").iter().next();

        assert!(token.is_none(), "Input was {:?}", token);
    }

    lexer_test!(FAIL: handles_extraneous_token, "@");
    lexer_test!(FAIL: handles_partially_consumend_input, "let abc @");

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
    lexer_test!(string, "\"abc\"" => TokenKind::String("abc".to_string()));
    lexer_test!(string_emoji, "\"🫡👨‍👩‍👧‍👦\"" => TokenKind::String("🫡👨‍👩‍👧‍👦".to_string()));
    lexer_test!(string_with_leading_number, "\"123abc\"" => TokenKind::String("123abc".to_string()));
    lexer_test!(string_with_special_chars, "\"123\nabc\"" => TokenKind::String("123\nabc".to_string()));
    lexer_test!(integer, "7" => TokenKind::Integer("7".to_string()));
    lexer_test!(integer_multiple_digits, "42069" => TokenKind::Integer("42069".to_string()));
    lexer_test!(float, "420.69" => TokenKind::Float("420.69".to_string()));
    lexer_test!(float_stops_after_second_dot, "192.168.0" => TokenKind::Float("192.168".to_string()));
}
