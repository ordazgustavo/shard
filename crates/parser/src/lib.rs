use std::iter::Peekable;

use lexer::{Span, Token, TokenKind};

#[derive(Debug)]
pub struct TokenMeta {
    pub kind: TokenKind,
    pub span: Span,
}

impl<'a> From<Token<'a>> for TokenMeta {
    fn from(value: Token<'a>) -> Self {
        Self {
            kind: value.kind,
            span: value.span,
        }
    }
}

pub struct Program(pub Vec<Stmt>);

impl FromIterator<Stmt> for Program {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Program(iter.into_iter().collect())
    }
}

#[derive(Debug)]
pub enum Stmt {
    Error(ParserError),

    Let(LetStmt),
    Fn(FnStmt),
}

#[derive(Debug)]
pub enum ParserError {
    MissingToken(TokenKind),
    UnexpectedToken {
        unexpected: TokenMeta,
        expected: TokenKind,
    },
    ExpectedExpr(TokenMeta),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct LetStmt {
    let_key: TokenMeta,
    ty: Option<TokenMeta>,
    name: Ident,
    equal: TokenMeta,
    expr: Expr,
    semi: TokenMeta,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct FnStmt {
    fn_key: TokenMeta,
    name: Ident,
    lparen: TokenMeta,
    rparen: TokenMeta,
    lbrace: TokenMeta,
    body: Vec<Stmt>,
    rbrace: TokenMeta,
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    String(String),
    Integer(String),
    Float(String),
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
            tokens: self.tokens.peekable(),
            exhausted: false,
        }
    }
}

impl<'a, T> IntoIterator for Parser<T>
where
    T: Iterator<Item = Token<'a>>,
{
    type Item = Stmt;

    type IntoIter = ParserIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    tokens: Peekable<T>,
    exhausted: bool,
}

impl<'a, T> ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    fn ensure_token(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let Some(token) = self.tokens.next() else { return Err(ParserError::MissingToken(kind)) };
        if token.kind != kind {
            return Err(ParserError::UnexpectedToken {
                unexpected: token.into(),
                expected: kind,
            });
        }
        Ok(token)
    }

    fn ensure_expr(&mut self) -> Result<Expr, ParserError> {
        let Some(token) = self.tokens.next() else { todo!() };

        match token.kind {
            TokenKind::Ident => Ok(Expr::Ident(Ident {
                name: token.text.to_string(),
            })),
            TokenKind::String => Ok(Expr::String(token.text.to_string())),
            TokenKind::Integer => Ok(Expr::Integer(token.text.to_string())),
            TokenKind::Float => Ok(Expr::Integer(token.text.to_string())),
            _ => Err(ParserError::ExpectedExpr(token.into())),
        }
    }

    fn ensure_ident(&mut self) -> Result<Ident, ParserError> {
        let ident = self.ensure_token(TokenKind::Ident)?;

        Ok(Ident {
            name: ident.text.to_string(),
        })
    }

    fn handle_let(&mut self, token: Token) -> Result<Stmt, ParserError> {
        Ok(Stmt::Let(LetStmt {
            let_key: token.into(),
            ty: None,
            name: self.ensure_ident()?,
            equal: self.ensure_token(TokenKind::Equal)?.into(),
            expr: self.ensure_expr()?,
            semi: self.ensure_token(TokenKind::Semicolon)?.into(),
        }))
    }

    fn collect_body(&mut self) -> Vec<Stmt> {
        let mut body = vec![];
        while self
            .tokens
            .peek()
            .map_or(false, |t| t.kind != TokenKind::RBrace)
        {
            match self.next() {
                Some(stmt) => body.push(stmt),
                None => break,
            };
        }
        body
    }

    fn handle_fn(&mut self, token: Token) -> Result<Stmt, ParserError> {
        Ok(Stmt::Fn(FnStmt {
            fn_key: token.into(),
            name: self.ensure_ident()?,
            lparen: self.ensure_token(TokenKind::LParen)?.into(),
            rparen: self.ensure_token(TokenKind::RParen)?.into(),
            lbrace: self.ensure_token(TokenKind::LBrace)?.into(),
            body: self.collect_body(),
            rbrace: self.ensure_token(TokenKind::RBrace)?.into(),
        }))
    }
}

impl<'a, T> Iterator for ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let token = self.tokens.next()?;

        let stmt = match token.kind {
            lexer::TokenKind::Unknown => todo!(),
            lexer::TokenKind::Eof => return None,
            lexer::TokenKind::Ident => todo!(),
            lexer::TokenKind::String => todo!(),
            lexer::TokenKind::Integer => todo!(),
            lexer::TokenKind::Float => todo!(),
            lexer::TokenKind::Let => self.handle_let(token),
            lexer::TokenKind::Fn => self.handle_fn(token),
            lexer::TokenKind::Type => todo!(),
            lexer::TokenKind::Struct => todo!(),
            lexer::TokenKind::Comma => todo!(),
            lexer::TokenKind::Colon => todo!(),
            lexer::TokenKind::Semicolon => todo!(),
            lexer::TokenKind::LParen => todo!(),
            lexer::TokenKind::RParen => todo!(),
            lexer::TokenKind::LBrace => todo!(),
            lexer::TokenKind::RBrace => todo!(),
            lexer::TokenKind::LBracket => todo!(),
            lexer::TokenKind::RBracket => todo!(),
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
        };

        match stmt {
            Ok(stmt) => Some(stmt),
            Err(p_error) => {
                self.exhausted = true;
                Some(Stmt::Error(p_error))
            }
        }
    }
}
