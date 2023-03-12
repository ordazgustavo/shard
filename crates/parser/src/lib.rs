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

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken {
        unexpected: TokenMeta,
        expected: Option<TokenKind>,
    },
    ExpectedExpr(TokenMeta),
}

type PResult<T> = Result<T, ParserError>;

fn todo_token<T>(token: Token) -> PResult<T> {
    Err(ParserError::UnexpectedToken {
        unexpected: token.into(),
        expected: None,
    })
}

pub struct Program(pub Vec<Decl>);

impl FromIterator<Decl> for Program {
    fn from_iter<T: IntoIterator<Item = Decl>>(iter: T) -> Self {
        Program(iter.into_iter().collect())
    }
}

#[derive(Debug)]
pub enum Decl {
    Error(ParserError),

    Stmt(Stmt),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Stmt {
    Let {
        let_key: TokenMeta,
        ty: Option<TokenMeta>,
        name: Ident,
        equal: TokenMeta,
        expr: Expr,
        semi: TokenMeta,
    },
    Fn {
        fn_key: TokenMeta,
        name: Ident,
        lparen: TokenMeta,
        args: Vec<(Ident, Ident)>,
        rparen: TokenMeta,
        ret_ty: Option<Ident>,
        lbrace: TokenMeta,
        body: Vec<Decl>,
        rbrace: TokenMeta,
    },
    Struct {
        struct_key: TokenMeta,
        name: Ident,
        lbrace: TokenMeta,
        props: Vec<(Ident, Ident)>,
        rbrace: TokenMeta,
    },
    Enum {
        enum_key: TokenMeta,
        name: Ident,
        lbrace: TokenMeta,
        members: Vec<Ident>,
        rbrace: TokenMeta,
    },

    Expr {
        expr: Expr,
        semi: TokenMeta,
    },

    Return {
        ret: TokenMeta,
        expr: Option<Expr>,
        semi: TokenMeta,
    },
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
    Boolean(bool),

    Call {
        name: Ident,
        lparen: TokenMeta,
        args: Vec<Expr>,
        rparen: TokenMeta,
    },

    Infix {
        left: Box<Expr>,
        operator: Operator,
        right: Box<Expr>,
    },
    Prefix {
        operator: Operator,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    EqualTo,

    // Prefix
    Bang,

    // Comparison
    NotEqualTo,
    Or,
    And,
    GreaterThan,
    GreaterThanOrEqual,
    LesserThan,
    LesserThanOrEqual,
}

pub struct Parser<T> {
    tokens: T,
}

impl<'a, T> Parser<T>
where
    T: Iterator<Item = Token<'a>>,
{
    pub fn new<U>(iter: U) -> Self
    where
        U: IntoIterator<IntoIter = T>,
    {
        Self {
            tokens: iter.into_iter(),
        }
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
    type Item = Decl;

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

    fn token_is_operator(token: &Token) -> bool {
        matches!(
            token.kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Mul
                | TokenKind::Div
                | TokenKind::Bang
                | TokenKind::EqualTo
                | TokenKind::NotEqualTo
                | TokenKind::Or
                | TokenKind::And
                | TokenKind::GreaterThan
                | TokenKind::GreaterThanOrEqual
                | TokenKind::LesserThan
                | TokenKind::LesserThanOrEqual
        )
    }

    fn next_is_operator(&mut self) -> bool {
        self.tokens
            .peek()
            .map_or(false, |token| Self::token_is_operator(token))
    }

    fn next_if(&mut self, kind: TokenKind) -> Option<Token> {
        self.tokens.next_if(|token| token.kind == kind)
    }

    fn token_to_operator(token: Token) -> PResult<Operator> {
        let op = match token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Mul => Operator::Mul,
            TokenKind::Div => Operator::Div,
            TokenKind::Bang => Operator::Bang,
            TokenKind::EqualTo => Operator::EqualTo,
            TokenKind::NotEqualTo => Operator::NotEqualTo,
            TokenKind::Or => Operator::Or,
            TokenKind::And => Operator::And,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::GreaterThanOrEqual => Operator::GreaterThanOrEqual,
            TokenKind::LesserThan => Operator::LesserThan,
            TokenKind::LesserThanOrEqual => Operator::LesserThanOrEqual,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    unexpected: token.into(),
                    expected: None,
                })
            }
        };

        Ok(op)
    }

    fn ensure_operator(&mut self) -> PResult<Operator> {
        let token = self.tokens.next().expect("Expected to find an operator");

        Self::token_to_operator(token)
    }

    fn collect_call_args(&mut self) -> PResult<Vec<Expr>> {
        let mut args = vec![];
        while !self.next_is(TokenKind::RParen) {
            args.push(self.ensure_expr()?);
            self.next_if(TokenKind::Comma);
        }

        Ok(args)
    }

    fn token_to_expr(&mut self, token: Token) -> PResult<Expr> {
        if Self::token_is_operator(&token) {
            return self.token_to_prefix(token);
        }

        if self.next_is(TokenKind::LParen) {
            return Ok(Expr::Call {
                name: Ident {
                    name: token.text.to_string(),
                },
                lparen: self.ensure_token(TokenKind::LParen)?.into(),
                args: self.collect_call_args()?,
                rparen: self.ensure_token(TokenKind::RParen)?.into(),
            });
        }

        let left = match token.kind {
            TokenKind::Ident => Expr::Ident(Ident {
                name: token.text.to_string(),
            }),
            TokenKind::String => Expr::String(token.text.to_string()),
            TokenKind::Integer => Expr::Integer(token.text.to_string()),
            TokenKind::Float => Expr::Float(token.text.to_string()),
            TokenKind::True => Expr::Boolean(true),
            TokenKind::False => Expr::Boolean(false),
            _ => return Err(ParserError::ExpectedExpr(token.into())),
        };

        if self.next_is_operator() {
            let operator = self.ensure_operator()?;
            let right = self.ensure_expr()?;

            return Ok(Expr::Infix {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn ensure_expr(&mut self) -> PResult<Expr> {
        let token = self.tokens.next().expect("Expected to find an Expr");

        self.token_to_expr(token)
    }

    fn ensure_token(&mut self, kind: TokenKind) -> PResult<Token> {
        // Panic because it's an implementation error, we should always hit Eof before None
        let token = self.tokens.next().expect(&format!("expected token {kind}"));
        if token.kind != kind {
            return Err(ParserError::UnexpectedToken {
                unexpected: token.into(),
                expected: Some(kind),
            });
        }
        Ok(token)
    }

    fn ensure_ident(&mut self) -> PResult<Ident> {
        let ident = self.ensure_token(TokenKind::Ident)?;

        Ok(Ident {
            name: ident.text.to_string(),
        })
    }

    fn handle_expr(&mut self, token: Token) -> PResult<Decl> {
        let expr = self.token_to_expr(token)?;
        match self.next_if(TokenKind::Semicolon) {
            Some(semi) => Ok(Decl::Stmt(Stmt::Expr {
                expr,
                semi: semi.into(),
            })),
            None => Ok(Decl::Expr(expr)),
        }
    }

    fn handle_let(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Stmt(Stmt::Let {
            let_key: token.into(),
            ty: None,
            name: self.ensure_ident()?,
            equal: self.ensure_token(TokenKind::Equal)?.into(),
            expr: self.ensure_expr()?,
            semi: self.ensure_token(TokenKind::Semicolon)?.into(),
        }))
    }

    fn collect_body(&mut self) -> PResult<Vec<Decl>> {
        let mut body = vec![];
        while !self.next_is(TokenKind::RBrace) {
            match self.next() {
                Some(Decl::Error(error)) => return Err(error),
                Some(stmt) => body.push(stmt),
                None => break,
            };
        }
        Ok(body)
    }

    fn collect_args(&mut self) -> PResult<Vec<(Ident, Ident)>> {
        let mut idents = vec![];
        while !self.next_is(TokenKind::RParen) {
            if let Ok(id) = self.ensure_ident() {
                self.ensure_token(TokenKind::Colon)?;
                let ty = self.ensure_ident()?;
                idents.push((id, ty));
            } else {
                break;
            };

            if self.next_is(TokenKind::Comma) {
                self.ensure_token(TokenKind::Comma)?;
            }
        }
        Ok(idents)
    }

    fn get_ret_ty(&mut self) -> PResult<Option<Ident>> {
        if !self.next_is(TokenKind::ThinArrow) {
            return Ok(None);
        }
        self.ensure_token(TokenKind::ThinArrow)?;
        let ret_ty = self.ensure_ident()?;
        Ok(Some(ret_ty))
    }

    fn handle_fn(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Stmt(Stmt::Fn {
            fn_key: token.into(),
            name: self.ensure_ident()?,
            lparen: self.ensure_token(TokenKind::LParen)?.into(),
            args: self.collect_args()?,
            rparen: self.ensure_token(TokenKind::RParen)?.into(),
            ret_ty: self.get_ret_ty()?,
            lbrace: self.ensure_token(TokenKind::LBrace)?.into(),
            body: self.collect_body()?,
            rbrace: self.ensure_token(TokenKind::RBrace)?.into(),
        }))
    }

    fn collect_props(&mut self) -> PResult<Vec<(Ident, Ident)>> {
        let mut idents = vec![];
        while !self.next_is(TokenKind::RBrace) {
            if let Ok(id) = self.ensure_ident() {
                self.ensure_token(TokenKind::Colon)?;
                let ty = self.ensure_ident()?;
                idents.push((id, ty));
            } else {
                break;
            };

            if self.next_is(TokenKind::Comma) {
                self.ensure_token(TokenKind::Comma)?;
            }
        }
        Ok(idents)
    }

    fn handle_struct(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Stmt(Stmt::Struct {
            struct_key: token.into(),
            name: self.ensure_ident()?,
            lbrace: self.ensure_token(TokenKind::LBrace)?.into(),
            props: self.collect_props()?,
            rbrace: self.ensure_token(TokenKind::RBrace)?.into(),
        }))
    }

    fn collect_enum_members(&mut self) -> PResult<Vec<Ident>> {
        let mut members = vec![];
        while self.next_is(TokenKind::Ident) {
            members.push(self.ensure_ident()?);

            self.next_if(TokenKind::Comma);
        }
        Ok(members)
    }

    fn handle_enum(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Stmt(Stmt::Enum {
            enum_key: token.into(),
            name: self.ensure_ident()?,
            lbrace: self.ensure_token(TokenKind::LBrace)?.into(),
            members: self.collect_enum_members()?,
            rbrace: self.ensure_token(TokenKind::RBrace)?.into(),
        }))
    }

    fn handle_return(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Stmt(Stmt::Return {
            ret: token.into(),
            expr: self.ensure_expr()?.into(),
            semi: self.ensure_token(TokenKind::Semicolon)?.into(),
        }))
    }

    fn token_to_prefix(&mut self, token: Token) -> PResult<Expr> {
        Ok(Expr::Prefix {
            operator: Self::token_to_operator(token)?,
            right: self.ensure_expr()?.into(),
        })
    }

    fn handle_prefix(&mut self, token: Token) -> PResult<Decl> {
        Ok(Decl::Expr(self.token_to_prefix(token)?))
    }
}

impl<'a, T> Iterator for ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    type Item = Decl;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let token = self.tokens.next()?;

        let stmt = match token.kind {
            TokenKind::Unknown => todo_token(token),
            TokenKind::Eof => return None,
            TokenKind::Ident => self.handle_expr(token),
            TokenKind::String => self.handle_expr(token),
            TokenKind::Integer => self.handle_expr(token),
            TokenKind::Float => self.handle_expr(token),
            TokenKind::True => self.handle_expr(token),
            TokenKind::False => self.handle_expr(token),
            TokenKind::Let => self.handle_let(token),
            TokenKind::Fn => self.handle_fn(token),
            TokenKind::Type => todo_token(token),
            TokenKind::Struct => self.handle_struct(token),
            TokenKind::Enum => self.handle_enum(token),
            TokenKind::Return => self.handle_return(token),
            TokenKind::Comma => todo_token(token),
            TokenKind::Colon => todo_token(token),
            TokenKind::Semicolon => todo_token(token),
            TokenKind::LParen => todo_token(token),
            TokenKind::RParen => todo_token(token),
            TokenKind::LBrace => todo_token(token),
            TokenKind::RBrace => todo_token(token),
            TokenKind::LBracket => todo_token(token),
            TokenKind::RBracket => todo_token(token),
            TokenKind::ThinArrow => todo_token(token),
            TokenKind::Equal => todo_token(token),
            TokenKind::Plus => todo_token(token),
            TokenKind::Minus => todo_token(token),
            TokenKind::Mul => todo_token(token),
            TokenKind::Div => todo_token(token),
            TokenKind::Dot => todo_token(token),
            TokenKind::EqualTo => todo_token(token),
            TokenKind::Bang => self.handle_prefix(token),
            TokenKind::NotEqualTo => todo_token(token),
            TokenKind::Or => todo_token(token),
            TokenKind::And => todo_token(token),
            TokenKind::GreaterThan => todo_token(token),
            TokenKind::GreaterThanOrEqual => todo_token(token),
            TokenKind::LesserThan => todo_token(token),
            TokenKind::LesserThanOrEqual => todo_token(token),
        };

        match stmt {
            Ok(stmt) => Some(stmt),
            Err(p_error) => {
                self.exhausted = true;
                Some(Decl::Error(p_error))
            }
        }
    }
}
