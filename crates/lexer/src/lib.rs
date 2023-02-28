use std::{borrow::Borrow, fmt::Display, iter::Peekable, ops::Range, str::CharIndices};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
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
    DblEqual,
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
    pub span: Range<usize>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Range<usize>) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug)]
pub enum LexError {
    Empty,
    UnknownToken(usize),
}

pub struct Lexer<'a> {
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        if self.src.is_empty() {
            return Err(LexError::Empty);
        }

        let lexer_iter = LexerIter::new(self.src);

        let mut tokens = Vec::new();
        for token in lexer_iter {
            tokens.push(token?);
        }
        let eof = match tokens.last() {
            Some(t) => Token::new(TokenKind::Eof, t.span.end..t.span.end),
            None => Token::new(TokenKind::Eof, 0..0),
        };
        tokens.push(eof);

        Ok(tokens)
    }
}

struct LexerIter<'a> {
    src: Peekable<CharIndices<'a>>,
}

impl<'a> LexerIter<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src: src.char_indices().peekable(),
        }
    }
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some((idx, ch)) = self.src.peek() else { return None };

        match ch {
            c if c.is_whitespace() => {
                // TODO: Is there a better way to consume the iterator conditionally?
                while self.src.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}
                self.next()
            }
            '(' => as_token(&mut self.src, TokenKind::RParen),
            ')' => as_token(&mut self.src, TokenKind::LParen),
            '{' => as_token(&mut self.src, TokenKind::RBrace),
            '}' => as_token(&mut self.src, TokenKind::LBrace),
            '[' => as_token(&mut self.src, TokenKind::RBracket),
            ']' => as_token(&mut self.src, TokenKind::LBracket),
            '.' => as_token(&mut self.src, TokenKind::Dot),
            ',' => as_token(&mut self.src, TokenKind::Comma),
            ':' => as_token(&mut self.src, TokenKind::Colon),
            ';' => as_token(&mut self.src, TokenKind::Semicolon),
            '+' => as_token(&mut self.src, TokenKind::Plus),
            '-' | '=' => Some(Ok(as_multi_char_token(&mut self.src))),
            '"' => Some(Ok(string(&mut self.src))),
            '0'..='9' => Some(Ok(number(&mut self.src))),
            c @ '_' | c if c.is_alphabetic() => Some(Ok(ident(&mut self.src))),
            _ => Some(Err(LexError::UnknownToken(*idx))),
        }
    }
}

#[inline]
fn as_token(src: &mut Peekable<CharIndices>, kind: TokenKind) -> Option<Result<Token, LexError>> {
    src.next()
        .map(|(idx, _)| Ok(Token::new(kind, idx..idx + 1)))
}

#[inline]
fn as_multi_char_token(src: &mut Peekable<CharIndices>) -> Token {
    let (start, ch1) = src.next().unwrap();

    match (ch1, src.next_if(|(_, ch)| !ch.is_whitespace())) {
        ('-', Some((end, '>'))) => Token::new(TokenKind::ThinArrow, start..end + 1),
        ('-', None) => Token::new(TokenKind::Minus, start..start + 1),
        ('=', Some((end, '='))) => Token::new(TokenKind::DblEqual, start..end + 1),
        ('=', None) => Token::new(TokenKind::Equal, start..start + 1),
        _ => unreachable!("How?!"),
    }
}

#[inline]
fn ident(src: &mut Peekable<CharIndices>) -> Token {
    let (start, ch) = src.next().unwrap();
    let mut end = start + 1;

    let mut id = String::new();
    id.push(ch);

    while let Some((idx, ch)) = src.next_if(|(_, ch)| *ch == '_' || ch.is_alphanumeric()) {
        id.push(ch);
        end = idx + 1;
    }

    let range = start..end;

    match id.borrow() {
        "let" => Token::new(TokenKind::Let, range),
        "fn" => Token::new(TokenKind::Fn, range),
        "type" => Token::new(TokenKind::Type, range),
        "struct" => Token::new(TokenKind::Struct, range),
        _ => Token::new(id.into(), range),
    }
}

#[inline]
fn string(src: &mut Peekable<CharIndices>) -> Token {
    let (start, _) = src.next().unwrap();
    let mut end = start + 1;

    let mut s = String::new();

    for (idx, ch) in src {
        end = idx + 1;
        if ch == '"' {
            break;
        }
        s.push(ch);
    }

    Token::new(TokenKind::String(s), start..end)
}

#[inline]
fn number(src: &mut Peekable<CharIndices>) -> Token {
    let (start, ch) = src.next().unwrap();
    let mut end = start + 1;

    let mut s = String::new();
    s.push(ch);

    let mut is_float = false;
    while let Some((idx, ch)) = src.next_if(|(_, ch)| *ch == '.' || ch.is_digit(10)) {
        end = idx + 1;
        s.push(ch);
        if is_float {
            if let Some((_, ch)) = src.peek() {
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
        Token::new(TokenKind::Float(s), start..end)
    } else {
        Token::new(TokenKind::Integer(s), start..end)
    }
}

#[cfg(test)]
mod tests {
    use super::TokenKind;
    macro_rules! lexer_test {
        (FAIL: $name:ident, $src:expr) => {
            #[test]
            fn $name() {
                use super::Lexer;
                let tokens = Lexer::new($src).lex();

                assert!(tokens.is_err(), "{:?} should be an error", tokens);
            }
        };
        ($name:ident, $src:expr => $should_be:expr) => {
            #[test]
            fn $name() {
                use super::Lexer;
                let tokens = Lexer::new($src).lex();

                assert!(tokens.is_ok(), "Should not be error {tokens:?} {}", $src);

                let token = tokens.unwrap().first().unwrap().kind.clone();

                assert_eq!(token, $should_be, "Input was {}", $src);
            }
        };
    }

    lexer_test!(FAIL: handles_empty_input, "");
    lexer_test!(FAIL: handles_extraneous_token, "@");
    lexer_test!(FAIL: handles_partially_consumend_input, "let abc @");

    lexer_test!(rparen_punctuation, "(" => TokenKind::RParen);
    lexer_test!(lparen_punctuation, ")" => TokenKind::LParen);
    lexer_test!(rbrace_punctuation, "{" => TokenKind::RBrace);
    lexer_test!(lbrace_punctuation, "}" => TokenKind::LBrace);
    lexer_test!(rbracket_punctuation, "[" => TokenKind::RBracket);
    lexer_test!(lbracket_punctuation, "]" => TokenKind::LBracket);
    lexer_test!(equal_punctuation, "=" => TokenKind::Equal);
    lexer_test!(dbl_equal_punctuation, "==" => TokenKind::DblEqual);
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
    lexer_test!(string_with_leading_number, "\"123abc\"" => TokenKind::String("123abc".to_string()));
    lexer_test!(string_with_special_chars, "\"123\nabc\"" => TokenKind::String("123\nabc".to_string()));
    lexer_test!(integer, "7" => TokenKind::Integer("7".to_string()));
    lexer_test!(integer_multiple_digits, "42069" => TokenKind::Integer("42069".to_string()));
    lexer_test!(float, "420.69" => TokenKind::Float("420.69".to_string()));
    lexer_test!(float_stops_after_second_dot, "192.168.0" => TokenKind::Float("192.168".to_string()));
}
