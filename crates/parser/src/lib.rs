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
    Expr(Expr),
}

#[derive(Debug)]
pub enum ParserError {
    MissingToken(TokenKind),
    UnexpectedToken {
        unexpected: TokenMeta,
        expected: Option<TokenKind>,
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
    args: Vec<Ident>,
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

    Infix(InfixExpr),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct InfixExpr {
    left: Box<Expr>,
    operator: Operator,
    right: Box<Expr>,
}

#[derive(Debug)]
pub enum Operator {
    Plus,
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
    fn next_is(&mut self, kind: TokenKind) -> bool {
        self.tokens.peek().map_or(false, |t| t.kind == kind)
    }

    fn token_to_expr(token: Token) -> Result<Expr, ParserError> {
        match token.kind {
            TokenKind::Ident => Ok(Expr::Ident(Ident {
                name: token.text.to_string(),
            })),
            TokenKind::String => Ok(Expr::String(token.text.to_string())),
            TokenKind::Integer => Ok(Expr::Integer(token.text.to_string())),
            TokenKind::Float => Ok(Expr::Float(token.text.to_string())),
            _ => Err(ParserError::ExpectedExpr(token.into())),
        }
    }

    fn ensure_operator(&mut self) -> Result<Operator, ParserError> {
        let Some(token) = self.tokens.next() else { todo!() };

        let op = match token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => todo!(),
            TokenKind::Mul => todo!(),
            TokenKind::Div => todo!(),
            TokenKind::Dot => todo!(),
            TokenKind::EqualTo => todo!(),
            TokenKind::Bang => todo!(),
            TokenKind::NotEqualTo => todo!(),
            TokenKind::Or => todo!(),
            TokenKind::And => todo!(),
            TokenKind::GreaterThan => todo!(),
            TokenKind::GreaterThanOrEqual => todo!(),
            TokenKind::LesserThan => todo!(),
            TokenKind::LesserThanOrEqual => todo!(),
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token.into(),
                    expected: None,
                })
            }
        };

        Ok(op)
    }

    fn handle_expr_stmt(&mut self, token: Token) -> Result<Stmt, ParserError> {
        let left = Self::token_to_expr(token)?;

        if self.next_is(TokenKind::Plus) {
            let operator = self.ensure_operator()?;
            let right = self.ensure_expr()?;
            return Ok(Stmt::Expr(Expr::Infix(InfixExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            })));
        }

        Ok(Stmt::Expr(left))
    }

    fn ensure_expr(&mut self) -> Result<Expr, ParserError> {
        let Some(token) = self.tokens.next() else { todo!() };

        let left = Self::token_to_expr(token)?;
        if self.next_is(TokenKind::Plus) {
            let operator = self.ensure_operator()?;
            let right = self.ensure_expr()?;
            return Ok(Expr::Infix(InfixExpr {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }));
        }

        Ok(left)
    }

    fn ensure_token(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let Some(token) = self.tokens.next() else { return Err(ParserError::MissingToken(kind)) };
        if token.kind != kind {
            return Err(ParserError::UnexpectedToken {
                unexpected: token.into(),
                expected: Some(kind),
            });
        }
        Ok(token)
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

    fn collect_body(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut body = vec![];
        while !self.next_is(TokenKind::RBrace) {
            match self.next() {
                Some(Stmt::Error(error)) => return Err(error),
                Some(stmt) => body.push(stmt),
                None => break,
            };
        }
        Ok(body)
    }

    fn collect_args(&mut self) -> Result<Vec<Ident>, ParserError> {
        let mut idents = vec![];
        while !self.next_is(TokenKind::RParen) {
            let Ok(id) = self.ensure_ident() else { break };
            idents.push(id);

            if self.next_is(TokenKind::Comma) {
                self.ensure_token(TokenKind::Comma)?;
            }
        }
        Ok(idents)
    }

    fn handle_fn(&mut self, token: Token) -> Result<Stmt, ParserError> {
        Ok(Stmt::Fn(FnStmt {
            fn_key: token.into(),
            name: self.ensure_ident()?,
            lparen: self.ensure_token(TokenKind::LParen)?.into(),
            args: self.collect_args()?,
            rparen: self.ensure_token(TokenKind::RParen)?.into(),
            lbrace: self.ensure_token(TokenKind::LBrace)?.into(),
            body: self.collect_body()?,
            rbrace: self.ensure_token(TokenKind::RBrace)?.into(),
        }))
    }
}

fn todo_token(token: Token) -> Result<Stmt, ParserError> {
    Err(ParserError::UnexpectedToken {
        unexpected: token.into(),
        expected: None,
    })
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
            lexer::TokenKind::Unknown => todo_token(token),
            lexer::TokenKind::Eof => return None,
            lexer::TokenKind::Ident => self.handle_expr_stmt(token),
            lexer::TokenKind::String => self.handle_expr_stmt(token),
            lexer::TokenKind::Integer => self.handle_expr_stmt(token),
            lexer::TokenKind::Float => self.handle_expr_stmt(token),
            lexer::TokenKind::Let => self.handle_let(token),
            lexer::TokenKind::Fn => self.handle_fn(token),
            lexer::TokenKind::Type => todo_token(token),
            lexer::TokenKind::Struct => todo_token(token),
            lexer::TokenKind::Comma => todo_token(token),
            lexer::TokenKind::Colon => todo_token(token),
            lexer::TokenKind::Semicolon => todo_token(token),
            lexer::TokenKind::LParen => todo_token(token),
            lexer::TokenKind::RParen => todo_token(token),
            lexer::TokenKind::LBrace => todo_token(token),
            lexer::TokenKind::RBrace => todo_token(token),
            lexer::TokenKind::LBracket => todo_token(token),
            lexer::TokenKind::RBracket => todo_token(token),
            lexer::TokenKind::ThinArrow => todo_token(token),
            lexer::TokenKind::Equal => todo_token(token),
            lexer::TokenKind::Plus => todo_token(token),
            lexer::TokenKind::Minus => todo_token(token),
            lexer::TokenKind::Mul => todo_token(token),
            lexer::TokenKind::Div => todo_token(token),
            lexer::TokenKind::Dot => todo_token(token),
            lexer::TokenKind::EqualTo => todo_token(token),
            lexer::TokenKind::Bang => todo_token(token),
            lexer::TokenKind::NotEqualTo => todo_token(token),
            lexer::TokenKind::Or => todo_token(token),
            lexer::TokenKind::And => todo_token(token),
            lexer::TokenKind::GreaterThan => todo_token(token),
            lexer::TokenKind::GreaterThanOrEqual => todo_token(token),
            lexer::TokenKind::LesserThan => todo_token(token),
            lexer::TokenKind::LesserThanOrEqual => todo_token(token),
        };

        match stmt {
            Ok(stmt) => Some(stmt),
            Err(p_error) => {
                println!("exhausted");
                self.exhausted = true;
                Some(Stmt::Error(p_error))
            }
        }
    }
}
