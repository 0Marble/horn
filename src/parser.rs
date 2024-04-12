use std::{fmt::Display, rc::Rc};

use crate::program::{Clause, Enviroment, Expr, IdentKind, Program};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Loc {
    End,
    LineCol(usize, usize),
}

impl Loc {
    pub fn inc_line(&mut self) {
        match self {
            Loc::End => (),
            Loc::LineCol(l, c) => {
                *l += 1;
                *c = 0;
            }
        }
    }
    pub fn inc_col(&mut self) {
        match self {
            Loc::End => (),
            Loc::LineCol(_, c) => {
                *c += 1;
            }
        }
    }

    pub fn start() -> Self {
        Self::LineCol(1, 0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Semi(Loc),
    Arrow(Loc),
    Coma(Loc),
    Question(Loc),
    Lp(Loc),
    Rp(Loc),
    Var(String, Loc),
    Const(String, Loc),
    Backslash(Loc),
    Dot(Loc),
    Plus(Loc),
    Minus(Loc),
    Star(Loc),
    Div(Loc),
    Eq(Loc),
    Neq(Loc),
    Gt(Loc),
    Ge(Loc),
    Lt(Loc),
    Le(Loc),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Semi,
    Arrow,
    Coma,
    Question,
    Lp,
    Rp,
    Var,
    Const,
    Backslash,
    Dot,
    Plus,
    Minus,
    Star,
    Div,
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
}

impl Token {
    pub fn get_loc(&self) -> Loc {
        match self {
            Token::Semi(a) => *a,
            Token::Arrow(a) => *a,
            Token::Coma(a) => *a,
            Token::Question(a) => *a,
            Token::Lp(a) => *a,
            Token::Rp(a) => *a,
            Token::Var(_, a) => *a,
            Token::Const(_, a) => *a,
            Token::Backslash(a) => *a,
            Token::Dot(a) => *a,
            Token::Plus(a) => *a,
            Token::Minus(a) => *a,
            Token::Star(a) => *a,
            Token::Div(a) => *a,
            Token::Eq(a) => *a,
            Token::Neq(a) => *a,
            Token::Gt(a) => *a,
            Token::Ge(a) => *a,
            Token::Lt(a) => *a,
            Token::Le(a) => *a,
        }
    }
    pub fn get_type(&self) -> TokenType {
        match self {
            Token::Semi(_) => TokenType::Semi,
            Token::Arrow(_) => TokenType::Arrow,
            Token::Coma(_) => TokenType::Coma,
            Token::Question(_) => TokenType::Question,
            Token::Lp(_) => TokenType::Lp,
            Token::Rp(_) => TokenType::Rp,
            Token::Var(_, _) => TokenType::Var,
            Token::Const(_, _) => TokenType::Const,
            Token::Backslash(_) => TokenType::Backslash,
            Token::Dot(_) => TokenType::Dot,
            Token::Plus(_) => TokenType::Plus,
            Token::Minus(_) => TokenType::Minus,
            Token::Star(_) => TokenType::Star,
            Token::Div(_) => TokenType::Div,
            Token::Eq(_) => TokenType::Eq,
            Token::Neq(_) => TokenType::Neq,
            Token::Gt(_) => TokenType::Gt,
            Token::Ge(_) => TokenType::Ge,
            Token::Lt(_) => TokenType::Lt,
            Token::Le(_) => TokenType::Le,
        }
    }

    fn get_ident(&self) -> Option<&str> {
        match self {
            Token::Var(s, _) => Some(s),
            Token::Const(s, _) => Some(s),
            _ => None,
        }
    }

    fn as_ident(self) -> Option<String> {
        match self {
            Token::Var(s, _) => Some(s),
            Token::Const(s, _) => Some(s),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError {
    loc: Loc,
    err: ParseErrorKind,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PARSE ERROR: ")?;
        match &self.err {
            ParseErrorKind::InvalidToken => write!(f, "Invalid Token")?,
            ParseErrorKind::UnexpectedEnd => write!(f, "Unexpected End")?,
            ParseErrorKind::ExpectedAny(e, g) => write!(f, "Expected {e:?}, got {g:?}")?,
            ParseErrorKind::RedeclIdent(id) => write!(f, "Identifier \"{id}\" redeclated")?,
        }

        write!(f, ", at {:?}", self.loc)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseErrorKind {
    InvalidToken,
    UnexpectedEnd,
    ExpectedAny(Rc<[TokenType]>, TokenType),
    RedeclIdent(String),
}

pub fn tokenize(s: impl IntoIterator<Item = char>) -> Result<TokenStream, ParseError> {
    let mut toks = vec![];

    let mut state = 0;
    let mut ident = String::new();

    let mut loc = Loc::start();
    'OUTER: for c in s {
        if c == '\n' {
            loc.inc_line();
        }
        loc.inc_col();

        loop {
            match (state, c) {
                (0, '<') => state = 1,
                (1, '-') => {
                    state = 0;
                    toks.push(Some(Token::Arrow(loc)));
                }
                (1, '=') => {
                    state = 0;
                    toks.push(Some(Token::Le(loc)));
                }
                (1, _) => {
                    state = 0;
                    toks.push(Some(Token::Lt(loc)));
                    continue;
                }

                (0, ';') => toks.push(Some(Token::Semi(loc))),
                (0, ',') => toks.push(Some(Token::Coma(loc))),
                (0, '(') => toks.push(Some(Token::Lp(loc))),
                (0, ')') => toks.push(Some(Token::Rp(loc))),
                (0, '?') => toks.push(Some(Token::Question(loc))),
                (0, '\\') => toks.push(Some(Token::Backslash(loc))),
                (0, '+') => toks.push(Some(Token::Plus(loc))),
                (0, '-') => toks.push(Some(Token::Minus(loc))),
                (0, '*') => toks.push(Some(Token::Star(loc))),
                (0, '/') => toks.push(Some(Token::Div(loc))),
                (0, '.') => toks.push(Some(Token::Dot(loc))),
                (0, '=') => toks.push(Some(Token::Eq(loc))),

                (0, '>') => state = 3,
                (3, '=') => {
                    state = 0;
                    toks.push(Some(Token::Ge(loc)));
                }
                (3, _) => {
                    state = 0;
                    toks.push(Some(Token::Gt(loc)));
                    continue;
                }

                (0, _) if c == '_' || ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) => {
                    assert!(ident.is_empty());
                    ident.push(c);
                    state = 2;
                }
                (2, _)
                    if c == '_'
                        || ('a'..='z').contains(&c)
                        || ('A'..='Z').contains(&c)
                        || ('0'..='9').contains(&c) =>
                {
                    ident.push(c);
                }
                (2, _) => {
                    if ident.starts_with('_') {
                        toks.push(Some(Token::Const(std::mem::take(&mut ident), loc)));
                    } else {
                        toks.push(Some(Token::Var(std::mem::take(&mut ident), loc)));
                    }
                    state = 0;
                    continue;
                }
                (0, _) if c.is_whitespace() => continue 'OUTER,
                _ => {
                    return Err(ParseError {
                        loc,
                        err: ParseErrorKind::InvalidToken,
                    })
                }
            }
            break;
        }
    }

    Ok(TokenStream { index: 0, toks })
}

/*
* PROGRAM -> CLAUSES TASK;
* CLAUSES -> HORN ';' CLAUSES | ;
* HORN -> EXPR '<-' EXPRS;
* EXPRS -> EXPR ',' EXPRS | ;
* TASK -> '?' EXPRS;
* EXPR -> LHS RHS;
* RHS -> BINOP LHS RHS | ;
* BINOP -> '\' func | dot | plus | minus | ...;
* LHS -> var | const | func '(' EXPRS ');
*/

pub struct TokenStream {
    index: usize,
    toks: Vec<Option<Token>>,
}

impl TokenStream {
    pub fn eat_next(&mut self) -> Option<Token> {
        self.index += 1;
        self.toks.get_mut(self.index - 1).map(|t| t.take().unwrap())
    }
    pub fn peek_next(&self) -> Option<&Token> {
        self.toks.get(self.index).map(|t| t.as_ref().unwrap())
    }
    pub fn is_empty(&self) -> bool {
        self.index >= self.toks.len()
    }

    pub fn eat_expect_any(&mut self, opts: &[TokenType]) -> Result<Token, ParseError> {
        let t = self.eat_next().ok_or(ParseError {
            loc: Loc::End,
            err: ParseErrorKind::UnexpectedEnd,
        })?;

        if opts.contains(&t.get_type()) {
            Ok(t)
        } else {
            Err(ParseError {
                loc: t.get_loc(),
                err: ParseErrorKind::ExpectedAny(opts.to_vec().into(), t.get_type()),
            })
        }
    }
    pub fn peek_expect_any(&self, opts: &[TokenType]) -> Result<&Token, ParseError> {
        let t = self.peek_next().ok_or(ParseError {
            loc: Loc::End,
            err: ParseErrorKind::UnexpectedEnd,
        })?;

        if opts.contains(&t.get_type()) {
            Ok(t)
        } else {
            Err(ParseError {
                loc: t.get_loc(),
                err: ParseErrorKind::ExpectedAny(opts.to_vec().into(), t.get_type()),
            })
        }
    }
}

pub fn parse(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Program, ParseError> {
    let mut clauses = vec![];
    loop {
        clauses.push(parse_clause(toks, env)?);

        if let Some(Token::Question(_)) = toks.peek_next() {
            break;
        }
    }

    let task = parse_task(toks, env)?;

    Ok(Program::new(clauses, task))
}

fn parse_clause(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Clause, ParseError> {
    let lhs = parse_expr(toks, env)?;
    toks.eat_expect_any(&[TokenType::Arrow])?;

    let mut rhs = vec![];
    if let Some(Token::Semi(_)) = toks.peek_next() {
        toks.eat_next();
        return Ok(Clause::new(lhs, rhs));
    }

    loop {
        rhs.push(parse_expr(toks, env)?);
        let coma_or_semi = toks.eat_expect_any(&[TokenType::Coma, TokenType::Semi])?;
        if coma_or_semi.get_type() == TokenType::Semi {
            break;
        }
    }

    rhs.reverse();
    Ok(Clause::new(lhs, rhs))
}

fn parse_task(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Vec<Expr>, ParseError> {
    toks.eat_expect_any(&[TokenType::Question])?;

    let mut res = vec![];
    loop {
        res.push(parse_expr(toks, env)?);
        let t = toks.eat_expect_any(&[TokenType::Coma, TokenType::Semi])?;
        if t.get_type() == TokenType::Semi {
            break;
        }
    }

    res.reverse();
    Ok(res)
}

fn parse_expr(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Expr, ParseError> {
    let lhs = parse_lhs(toks, env)?;
    let expr = if let Some((op, rhs)) = parse_rhs(toks, env)? {
        Expr::Func(op, vec![lhs, rhs].into())
    } else {
        lhs
    };

    Ok(expr)
}

fn parse_lhs(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Expr, ParseError> {
    let tok = toks.eat_expect_any(&[TokenType::Var, TokenType::Const])?;

    if let Some(Token::Lp(_)) = toks.peek_next() {
        let args = parse_args(toks, env)?;
        let loc = tok.get_loc();
        let id = add_ident(
            env,
            tok.as_ident().unwrap(),
            IdentKind::Func(args.len()),
            loc,
        )?;
        return Ok(Expr::Func(id, args.into()));
    }

    match tok {
        Token::Var(s, l) => Ok(Expr::Var(add_ident(env, s, IdentKind::Var, l)?)),
        Token::Const(s, l) => Ok(Expr::Const(add_ident(env, s, IdentKind::Const, l)?)),
        _ => panic!("Unreached"),
    }
}

fn parse_rhs(
    toks: &mut TokenStream,
    env: &mut Enviroment,
) -> Result<Option<(usize, Expr)>, ParseError> {
    if let Some(binop) = toks
        .peek_expect_any(&[
            TokenType::Backslash,
            TokenType::Dot,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Div,
            TokenType::Eq,
            TokenType::Neq,
            TokenType::Gt,
            TokenType::Ge,
            TokenType::Lt,
            TokenType::Le,
        ])
        .ok()
    {
        let binop = toks.eat_next().expect("We already peeked it?");

        let op_name = if binop.get_type() == TokenType::Backslash {
            let ident = toks.eat_expect_any(&[TokenType::Var, TokenType::Const])?;
            ident.as_ident().expect("Is a var or a const")
        } else {
            match binop {
                Token::Dot(_) => ".".to_string(),
                Token::Plus(_) => "+".to_string(),
                Token::Minus(_) => "-".to_string(),
                Token::Star(_) => "*".to_string(),
                Token::Div(_) => "/".to_string(),
                Token::Eq(_) => "=".to_string(),
                Token::Neq(_) => "!=".to_string(),
                Token::Gt(_) => ">".to_string(),
                Token::Ge(_) => ">=".to_string(),
                Token::Lt(_) => "<".to_string(),
                Token::Le(_) => "<=".to_string(),
                _ => panic!("Unreachable"),
            }
        };

        let id = add_ident(env, op_name, IdentKind::Func(2), binop.get_loc())?;
        let rhs = parse_lhs(toks, env)?;
        let full_rhs = if let Some((next_op, next_expr)) = parse_rhs(toks, env)? {
            Expr::Func(next_op, vec![rhs, next_expr].into())
        } else {
            rhs
        };

        Ok(Some((id, full_rhs)))
    } else {
        Ok(None)
    }
}

fn add_ident(
    env: &mut Enviroment,
    ident: String,
    kind: IdentKind,
    loc: Loc,
) -> Result<usize, ParseError> {
    match (env.get_ident_kind(&ident), kind) {
        (None, kind) => Ok(env.add_ident(ident, kind)),
        (Some((old_id, old_kind)), kind) => {
            if kind == old_kind {
                Ok(old_id)
            } else {
                Err(ParseError {
                    loc,
                    err: ParseErrorKind::RedeclIdent(ident),
                })
            }
        }
    }
}

fn parse_args(toks: &mut TokenStream, env: &mut Enviroment) -> Result<Vec<Expr>, ParseError> {
    toks.eat_expect_any(&[TokenType::Lp])?;

    let mut res = vec![];
    loop {
        res.push(parse_expr(toks, env)?);
        let coma_or_rp = toks.eat_expect_any(&[TokenType::Coma, TokenType::Rp])?;
        if coma_or_rp.get_type() == TokenType::Rp {
            break;
        }
    }

    Ok(res)
}
