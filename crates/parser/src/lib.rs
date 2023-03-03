use lexer::{Token, TokenKind};

pub struct Program(pub Vec<Stmt>);

impl FromIterator<Stmt> for Program {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Program(iter.into_iter().collect())
    }
}

#[derive(Debug)]
pub enum Stmt {
    Let(Expr, Expr),
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    String(String),
}

pub struct Parser<T> {
    tokens: T,
}

impl<'a, T> Parser<T>
where
    T: Iterator<Item = Token<'a>>,
{
    pub fn new(tokens: T) -> Self {
        Self { tokens }
    }

    pub fn iter(self) -> ParserIter<'a, T> {
        ParserIter {
            tokens: self.tokens,
        }
    }
}

pub struct ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    tokens: T,
}

impl<'a, T> ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    fn handle_let(&mut self) -> Option<Stmt> {
        let Some(Token {kind: TokenKind::Ident, text: ident, ..} ) = self.tokens.next() else { return None };
        let Some(next) = self.tokens.next() else {return None};
        if next.kind != TokenKind::Equal {
            return None;
        }
        let Some(Token {kind: TokenKind::String, text: value, ..} ) = self.tokens.next() else { return None };
        Some(Stmt::Let(
            Expr::Ident(ident.to_string()),
            Expr::String(value.to_string()),
        ))
    }
}

impl<'a, T> Iterator for ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.tokens.next() else { return None };

        match token.kind {
            lexer::TokenKind::Unknown => todo!(),
            lexer::TokenKind::Eof => None,
            lexer::TokenKind::Ident => todo!(),
            lexer::TokenKind::String => todo!(),
            lexer::TokenKind::Integer => todo!(),
            lexer::TokenKind::Float => todo!(),
            lexer::TokenKind::Let => self.handle_let(),
            lexer::TokenKind::Fn => todo!(),
            lexer::TokenKind::Type => todo!(),
            lexer::TokenKind::Struct => todo!(),
            lexer::TokenKind::Comma => todo!(),
            lexer::TokenKind::Colon => todo!(),
            lexer::TokenKind::Semicolon => todo!(),
            lexer::TokenKind::RParen => todo!(),
            lexer::TokenKind::LParen => todo!(),
            lexer::TokenKind::RBrace => todo!(),
            lexer::TokenKind::LBrace => todo!(),
            lexer::TokenKind::RBracket => todo!(),
            lexer::TokenKind::LBracket => todo!(),
            lexer::TokenKind::ThinArrow => todo!(),
            lexer::TokenKind::Equal => todo!(),
            lexer::TokenKind::Plus => todo!(),
            lexer::TokenKind::Minus => todo!(),
            lexer::TokenKind::Mul => todo!(),
            lexer::TokenKind::Div => todo!(),
            lexer::TokenKind::Dot => todo!(),
            lexer::TokenKind::EqualTo => todo!(),
            lexer::TokenKind::Bang => todo!(),
            lexer::TokenKind::NotEqualTo => todo!(),
            lexer::TokenKind::Or => todo!(),
            lexer::TokenKind::And => todo!(),
            lexer::TokenKind::GreaterThan => todo!(),
            lexer::TokenKind::GreaterThanOrEqual => todo!(),
            lexer::TokenKind::LesserThan => todo!(),
            lexer::TokenKind::LesserThanOrEqual => todo!(),
        }
    }
}
