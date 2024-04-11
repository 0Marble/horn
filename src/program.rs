use std::{collections::HashMap, fmt::Debug, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Var(usize),
    Const(usize),
    Func(usize, Rc<[Expr]>),
}
impl Expr {
    pub fn top_ident(&self) -> usize {
        match self {
            Expr::Var(i) => *i,
            Expr::Const(i) => *i,
            Expr::Func(i, _) => *i,
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
    task: Rc<[Expr]>,
}

impl Program {
    pub fn new(clauses: Vec<Clause>, task: Vec<Expr>) -> Self {
        Self {
            clauses: clauses.into(),
            task: task.into(),
        }
    }

    pub fn clauses(&self) -> impl Iterator<Item = (usize, Clause)> + '_ {
        self.clauses.iter().cloned().enumerate()
    }

    pub fn tasks(&self) -> impl Iterator<Item = (usize, Expr)> + '_ {
        self.task.iter().cloned().enumerate()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub fn rename_vars(&mut self, c: &Clause) -> Clause {
        let mut names = HashMap::new();
        let lhs = self.rename(c.lhs(), &mut names);
        let mut rhs = vec![];
        for e in c.rhs() {
            rhs.push(self.rename(e, &mut names));
        }
        Clause::new(lhs, rhs)
    }

    fn rename(&mut self, e: Expr, names: &mut HashMap<usize, usize>) -> Expr {
        match e {
            Expr::Var(x) => Expr::Var(*names.entry(x).or_insert_with(|| {
                self.next_id += 1;
                self.next_id - 1
            })),
            Expr::Const(c) => Expr::Const(c),
            Expr::Func(f, args) => Expr::Func(
                f,
                args.iter()
                    .cloned()
                    .map(|e| self.rename(e, names))
                    .collect(),
            ),
        }
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
