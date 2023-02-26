use std::{borrow::Borrow, fmt::Display, iter::Peekable, ops::Range, str::CharIndices};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Eof,

    Ident(String),

    Let,
    Fn,
    Type,

    RParen,
    LParen,
    RBrace,
    LBrace,
    RBracket,
    LBracket,
    Semicolon,
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
        let next = self.src.peek();

        let Some((idx, ch)) = next else { return None };

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
            ';' => as_token(&mut self.src, TokenKind::Semicolon),
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
fn ident(src: &mut Peekable<CharIndices>) -> Token {
    let (start, ch) = src.next().unwrap();
    let mut end = 0;

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
        _ => Token::new(id.into(), range),
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

                assert!(tokens.is_ok());

                let token = tokens.unwrap().first().unwrap().kind.clone();

                assert_eq!(token, $should_be, "Input was {:?}", $src);
            }
        };
    }

    lexer_test!(FAIL: handles_empty_input, "");
    lexer_test!(FAIL: handles_extraneous_token, "@");
    lexer_test!(FAIL: handles_partially_consumend_input, "let abc @");

    lexer_test!(handles_rparen_punctuation, "(" => TokenKind::RParen);
    lexer_test!(handles_lparen_punctuation, ")" => TokenKind::LParen);
    lexer_test!(handles_rbrace_punctuation, "{" => TokenKind::RBrace);
    lexer_test!(handles_lbrace_punctuation, "}" => TokenKind::LBrace);
    lexer_test!(handles_rbracket_punctuation, "[" => TokenKind::RBracket);
    lexer_test!(handles_lbracket_punctuation, "]" => TokenKind::LBracket);
    lexer_test!(handles_semicolon_punctuation, ";" => TokenKind::Semicolon);

    lexer_test!(handles_let_keyword, "let" => TokenKind::Let);
    lexer_test!(handles_fn_keyword, "fn" => TokenKind::Fn);
    lexer_test!(handles_type_keyword, "type" => TokenKind::Type);

    lexer_test!(handles_identifiers, "abc" => TokenKind::Ident("abc".to_string()));
}
