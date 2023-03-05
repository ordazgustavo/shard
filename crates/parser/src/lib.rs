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
}

#[derive(Debug)]
pub enum ParserError {
    MissingToken(TokenKind),
    UnexpectedToken {
        unexpected: TokenMeta,
        expected: TokenKind,
    },
}

#[derive(Debug)]
pub struct LetStmt {
    l: TokenMeta,
    ty: Option<TokenMeta>,
    name: Ident,
    equal: TokenMeta,
    expr: Expr,
    semi: TokenMeta,
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
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
            exhausted: false,
        }
    }
}

pub struct ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    tokens: T,
    exhausted: bool,
}

impl<'a, T> ParserIter<'a, T>
where
    T: Iterator<Item = Token<'a>>,
{
    fn handle_let(&mut self, token: Token) -> Option<Stmt> {
        let ident = match ensure_token(self.tokens.next(), TokenKind::Ident) {
            Ok(t) => t,
            Err(e) => return Some(Stmt::Error(e)),
        };
        let equal = match ensure_token(self.tokens.next(), TokenKind::Equal) {
            Ok(t) => t,
            Err(e) => return Some(Stmt::Error(e)),
        };
        let value = match ensure_token(self.tokens.next(), TokenKind::String) {
            Ok(t) => t,
            Err(e) => return Some(Stmt::Error(e)),
        };
        let semi = match ensure_token(self.tokens.next(), TokenKind::Semicolon) {
            Ok(t) => t,
            Err(e) => return Some(Stmt::Error(e)),
        };

        Some(Stmt::Let(LetStmt {
            l: token.into(),
            ty: None,
            name: Ident {
                name: ident.text.to_string(),
            },
            equal: equal.into(),
            expr: Expr::String(value.text.to_string()),
            semi: semi.into(),
        }))
    }
}

fn ensure_token(token: Option<Token>, kind: TokenKind) -> Result<Token, ParserError> {
    let Some(token) = token else { return Err(ParserError::MissingToken(kind)) };
    if token.kind != kind {
        return Err(ParserError::UnexpectedToken {
            unexpected: token.into(),
            expected: kind,
        });
    }
    Ok(token)
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
        };

        if matches!(stmt, Some(Stmt::Error(_))) {
            self.exhausted = true;
        }
        return stmt;
    }
}
