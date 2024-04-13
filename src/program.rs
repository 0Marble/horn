use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    io::Write,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Var(usize),
    Const(usize),
    Func(usize, Rc<[Expr]>),
}

impl Expr {
    pub fn print(&self, w: &mut dyn Write, env: &Enviroment) -> std::io::Result<()> {
        match self {
            Expr::Var(x) => write!(w, "{}", env.get_ident_name(*x).unwrap()),
            Expr::Const(c) => write!(w, "{}", env.get_ident_name(*c).unwrap()),
            Expr::Func(f, args) => {
                write!(w, "{}(", env.get_ident_name(*f).unwrap())?;
                args[0].print(w, env)?;
                for i in 1..args.len() {
                    write!(w, ", ")?;
                    args[i].print(w, env)?;
                }
                write!(w, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Clause {
    lhs: Expr,
    rhs: Rc<[Expr]>,
}

impl Clause {
    pub fn new(lhs: Expr, rhs: Vec<Expr>) -> Self {
        Self {
            lhs,
            rhs: rhs.into(),
        }
    }

    pub fn lhs(&self) -> Expr {
        self.lhs.clone()
    }

    pub fn rhs(&self) -> impl Iterator<Item = Expr> + '_ {
        self.rhs.iter().cloned()
    }
}

#[derive(Debug)]
pub struct Program {
    clauses: Rc<[Clause]>,
}

impl Program {
    pub fn new(clauses: Vec<Clause>) -> Self {
        Self {
            clauses: clauses.into(),
        }
    }

    pub fn clauses(&self) -> impl Iterator<Item = Clause> + '_ {
        self.clauses.iter().cloned()
    }
}

#[derive(Debug)]
pub struct Task {
    task: Rc<[Expr]>,
}

impl Task {
    pub fn new(task: Vec<Expr>) -> Self {
        Self { task: task.into() }
    }

    pub fn exprs(&self) -> impl Iterator<Item = Expr> + '_ {
        self.task.iter().cloned()
    }

    fn get_vars(&self, e: &Expr, set: &mut HashSet<usize>) {
        match e {
            Expr::Var(x) => _ = set.insert(*x),
            Expr::Func(_, args) => args.iter().for_each(|e| self.get_vars(e, set)),
            _ => (),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IdentKind {
    Const,
    Var,
    Func(usize),
}

#[derive(Debug)]
pub struct Enviroment {
    idents: HashMap<Rc<str>, usize>,
    kinds: HashMap<usize, IdentKind>,
    inv_idents: HashMap<usize, Rc<str>>,
    next_id: usize,
}

impl Enviroment {
    pub fn get_ident_kind(&self, ident: &str) -> Option<(usize, IdentKind)> {
        self.idents
            .get(ident)
            .cloned()
            .map(|i| (i, self.kinds.get(&i).cloned().unwrap()))
    }
    pub fn get_ident_name(&self, id: usize) -> Option<&str> {
        self.inv_idents.get(&id).map(|s| s.as_ref())
    }

    pub fn add_ident(&mut self, ident: String, kind: IdentKind) -> usize {
        let ident: Rc<str> = ident.into();
        let id = *self.idents.entry(ident.clone()).or_insert_with(|| {
            self.next_id += 1;
            self.next_id - 1
        });
        self.inv_idents.insert(id, ident);
        self.kinds.insert(id, kind);
        id
    }

    pub fn new() -> Self {
        Self {
            idents: Default::default(),
            kinds: Default::default(),
            inv_idents: Default::default(),
            next_id: 0,
        }
    }
}
