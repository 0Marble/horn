use std::{collections::VecDeque, fmt::Display, rc::Rc};

use crate::program::{Clause, Enviroment, Expr, IdentKind, Program, Task};

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

pub struct TokenStream<I> {
    buf: VecDeque<Token>,
    it: I,
    loc: Loc,
}

impl<I> TokenStream<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(it: I) -> Self {
        Self {
            buf: VecDeque::new(),
            it,
            loc: Loc::start(),
        }
    }

    fn eat_char(&mut self, state: &mut usize, ident: &mut String) -> Result<bool, ParseError> {
        let c = if let Some(c) = self.it.next() {
            c
        } else {
            return Ok(false);
        };

        if c == '\n' {
            self.loc.inc_line();
        }
        self.loc.inc_col();
        loop {
            match (*state, c) {
                (0, '<') => *state = 1,
                (1, '-') => {
                    *state = 0;
                    self.buf.push_back(Token::Arrow(self.loc));
                }
                (1, '=') => {
                    *state = 0;
                    self.buf.push_back(Token::Le(self.loc));
                }
                (1, _) => {
                    *state = 0;
                    self.buf.push_back(Token::Lt(self.loc));
                    continue;
                }

                (0, ';') => self.buf.push_back(Token::Semi(self.loc)),
                (0, ',') => self.buf.push_back(Token::Coma(self.loc)),
                (0, '(') => self.buf.push_back(Token::Lp(self.loc)),
                (0, ')') => self.buf.push_back(Token::Rp(self.loc)),
                (0, '?') => self.buf.push_back(Token::Question(self.loc)),
                (0, '\\') => self.buf.push_back(Token::Backslash(self.loc)),
                (0, '+') => self.buf.push_back(Token::Plus(self.loc)),
                (0, '-') => self.buf.push_back(Token::Minus(self.loc)),
                (0, '*') => self.buf.push_back(Token::Star(self.loc)),
                (0, '/') => self.buf.push_back(Token::Div(self.loc)),
                (0, '.') => self.buf.push_back(Token::Dot(self.loc)),
                (0, '=') => self.buf.push_back(Token::Eq(self.loc)),

                (0, '>') => *state = 3,
                (3, '=') => {
                    *state = 0;
                    self.buf.push_back(Token::Ge(self.loc));
                }
                (3, _) => {
                    *state = 0;
                    self.buf.push_back(Token::Gt(self.loc));
                    continue;
                }

                (0, '!') => *state = 4,
                (4, '=') => {
                    *state = 0;
                    self.buf.push_back(Token::Neq(self.loc));
                }

                (0, '#') => *state = 5,
                (5, '\n') => *state = 0,
                (5, _) => (),

                (0, _) if c == '_' || ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) => {
                    assert!(ident.is_empty());
                    ident.push(c);
                    *state = 2;
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
                        self.buf
                            .push_back(Token::Const(std::mem::take(ident), self.loc));
                    } else {
                        self.buf
                            .push_back(Token::Var(std::mem::take(ident), self.loc));
                    }
                    *state = 0;
                    continue;
                }
                (0, _) if c.is_whitespace() => (),
                _ => {
                    return Err(ParseError {
                        loc: self.loc,
                        err: ParseErrorKind::InvalidToken,
                    })
                }
            }
            break;
        }

        Ok(true)
    }

    pub fn eat_next(&mut self) -> Result<Option<Token>, ParseError> {
        let mut ident = String::new();
        let mut state = 0;
        while self.buf.len() < 2 {
            if !self.eat_char(&mut state, &mut ident)? {
                break;
            }
        }
        Ok(self.buf.pop_front())
    }
    pub fn peek_next(&self) -> Option<&Token> {
        self.buf.front()
    }

    pub fn eat_expect_any(&mut self, opts: &[TokenType]) -> Result<Token, ParseError> {
        let t = self.eat_next()?.ok_or(ParseError {
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

impl Program {
    pub fn parse<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Self, ParseError>
    where
        I: Iterator<Item = char>,
    {
        parse_prog(toks, env)
    }
}

impl Task {
    pub fn parse<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Self, ParseError>
    where
        I: Iterator<Item = char>,
    {
        Ok(Self::new(parse_task(toks, env)?))
    }
}

fn parse_prog<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Program, ParseError>
where
    I: Iterator<Item = char>,
{
    let mut clauses = vec![];
    loop {
        clauses.push(parse_clause(toks, env)?);

        if let Some(Token::Question(_)) = toks.peek_next() {
            break;
        }
        if toks.peek_next().is_none() {
            break;
        }
    }

    Ok(Program::new(clauses))
}

fn parse_clause<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Clause, ParseError>
where
    I: Iterator<Item = char>,
{
    let lhs = parse_expr(toks, env)?;
    toks.eat_expect_any(&[TokenType::Arrow])?;

    let mut rhs = vec![];
    if let Some(Token::Semi(_)) = toks.peek_next() {
        toks.eat_next().expect("Just peeked");
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

fn parse_task<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Vec<Expr>, ParseError>
where
    I: Iterator<Item = char>,
{
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

fn parse_expr<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Expr, ParseError>
where
    I: Iterator<Item = char>,
{
    let lhs = parse_lhs(toks, env)?;
    let expr = if let Some((op, rhs)) = parse_rhs(toks, env)? {
        Expr::Func(op, vec![lhs, rhs].into())
    } else {
        lhs
    };

    Ok(expr)
}

fn parse_lhs<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Expr, ParseError>
where
    I: Iterator<Item = char>,
{
    let tok = toks.eat_expect_any(&[TokenType::Var, TokenType::Const, TokenType::Lp])?;

    if tok.get_type() == TokenType::Lp {
        let expr = parse_expr(toks, env)?;
        toks.eat_expect_any(&[TokenType::Rp])?;
        return Ok(expr);
    }

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

fn parse_rhs<I>(
    toks: &mut TokenStream<I>,
    env: &mut Enviroment,
) -> Result<Option<(usize, Expr)>, ParseError>
where
    I: Iterator<Item = char>,
{
    if let Some(_) = toks
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
        let binop = toks
            .eat_next()
            .expect("We already peeked it?")
            .expect("again");

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

fn parse_args<I>(toks: &mut TokenStream<I>, env: &mut Enviroment) -> Result<Vec<Expr>, ParseError>
where
    I: Iterator<Item = char>,
{
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
