use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::{
    disjoint_set::{DisjointSet, SetItem},
    program::{Enviroment, Expr, Program},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    DepthExceeded,
}

#[derive(Debug)]
pub struct Evaluator {
    prog: Program,
    env: Enviroment,
}

impl Evaluator {
    pub fn new(prog: Program, env: Enviroment) -> Self {
        Self { prog, env }
    }

    pub fn eval(&mut self, max_depth: usize) -> Result<(), EvalError> {
        let mut stack: Vec<(_, _, _, Vec<_>)> =
            vec![(0, vec![], 0, self.prog.tasks().map(|(_, e)| e).collect())];

        while let Some((d, mut subs, next_clause, mut task)) = stack.pop() {
            if d > max_depth {
                return Err(EvalError::DepthExceeded);
            }

            if self.prog.clauses().nth(next_clause + 1).is_some() {
                stack.push((d, subs.clone(), next_clause + 1, task.clone()));
            }
            let last_task = task.pop().unwrap();
            let (_, clause) = self.prog.clauses().nth(next_clause).unwrap();
            let clause = self.env.rename_vars(&clause);

            if let Some(sub) = Substitution::new(last_task.clone(), clause.lhs()) {
                task.extend(clause.rhs());
                for e in &mut task {
                    *e = sub.substitute(e);
                }

                subs.push(sub);

                if task.is_empty() {
                    self.print_res(&subs);
                    println!("=======SUCCESS========");
                } else {
                    stack.push((d + 1, subs, 0, task));
                }
            }
        }

        Ok(())
    }

    fn print_res(&self, subs: &[Substitution]) {
        let mut set = HashSet::new();

        for (_, e) in self.prog.tasks() {
            self.get_vars(&e, &mut set);
        }

        for e in set {
            self.print_expr(&Expr::Var(e));
            print!(" = ");
            let mut e = Expr::Var(e);
            for s in subs {
                e = s.substitute(&e);
            }
            self.print_expr(&e);
            println!();
        }
    }

    fn print_expr(&self, e: &Expr) {
        match e {
            Expr::Var(x) => print!("{}", self.env.get_ident_name(*x).unwrap()),
            Expr::Const(c) => print!("{}", self.env.get_ident_name(*c).unwrap()),
            Expr::Func(f, args) => {
                print!("{}(", self.env.get_ident_name(*f).unwrap());
                self.print_expr(&args[0]);
                for i in 1..args.len() {
                    print!(", ");
                    self.print_expr(&args[i]);
                }
                print!(")");
            }
        }
    }

    fn get_vars(&self, e: &Expr, set: &mut HashSet<usize>) {
        match e {
            Expr::Var(x) => _ = set.insert(*x),
            Expr::Func(_, args) => args.iter().for_each(|e| self.get_vars(e, set)),
            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
struct Substitution {
    set: DisjointSet<Expr>,
    items: HashMap<Expr, SetItem>,
}

impl Display for Substitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for repr in self.set.set_representatives() {
            writeln!(f, "{repr:?}")?;
            for e in self.set.set_of(repr).map(|e| self.set.get(e).unwrap()) {
                write!(f, "{e:?}, ")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Substitution {
    pub fn new(e1: Expr, e2: Expr) -> Option<Self> {
        let mut s = Self {
            set: DisjointSet::new(),
            items: HashMap::new(),
        };
        s.add_expr(e1.clone());
        s.add_expr(e2.clone());

        if s.unify(e1, e2) {
            Some(s)
        } else {
            None
        }
    }

    fn add_expr(&mut self, e: Expr) -> SetItem {
        match self.items.get(&e) {
            Some(i) => i.clone(),
            None => {
                if let Expr::Func(_, args) = &e {
                    for sub in args.iter() {
                        self.add_expr(sub.clone());
                    }
                }

                let id = self.set.add_set(e.clone());
                self.items.insert(e, id.clone());
                id
            }
        }
    }

    fn unify(&mut self, e1: Expr, e2: Expr) -> bool {
        let s = self.set.find(self.items.get(&e1).unwrap().clone()).unwrap();
        let t = self.set.find(self.items.get(&e2).unwrap().clone()).unwrap();

        if s == t {
            return true;
        }

        let merge_rule = |a: &Expr, b: &Expr| -> u8 {
            let res = match (a, b) {
                (Expr::Var(a), Expr::Var(b)) => {
                    if a < b {
                        1
                    } else {
                        2
                    }
                }
                (Expr::Var(_), Expr::Const(_)) => 2,
                (Expr::Var(_), Expr::Func(_, _)) => 2,
                (Expr::Const(_), Expr::Var(_)) => 1,
                (Expr::Const(_), Expr::Const(_)) => 0,
                (Expr::Func(_, _), Expr::Var(_)) => 1,
                (Expr::Func(_, _), Expr::Func(_, _)) => 0,
                _ => panic!("Should be unreachable"),
            };

            res
        };

        let e1 = self.set.get(s).unwrap().clone();
        let e2 = self.set.get(t).unwrap().clone();
        match (e1, e2) {
            (Expr::Var(_), Expr::Var(_)) => {
                self.set.merge(s, t, merge_rule);
                true
            }
            (Expr::Var(_), Expr::Const(_)) => {
                self.set.merge(s, t, merge_rule);
                true
            }
            (Expr::Const(_), Expr::Var(_)) => {
                self.set.merge(s, t, merge_rule);
                true
            }
            (Expr::Var(_), Expr::Func(_, _)) => {
                self.set.merge(s, t, merge_rule);
                true
            }
            (Expr::Const(a), Expr::Const(b)) => a == b,
            (Expr::Func(_, _), Expr::Var(_)) => {
                self.set.merge(s, t, merge_rule);
                true
            }
            (Expr::Func(f, args_1), Expr::Func(g, args_2)) => {
                if f != g {
                    return false;
                }

                assert_eq!(args_1.len(), args_2.len());
                self.set.merge(s, t, merge_rule);

                args_1
                    .iter()
                    .zip(args_2.iter())
                    .all(|(a, b)| self.unify(a.clone(), b.clone()))
            }
            _ => false,
        }
    }

    pub fn substitute(&self, e: &Expr) -> Expr {
        match e {
            Expr::Func(f, args) => {
                Expr::Func(*f, args.iter().map(|e| self.substitute(e)).collect())
            }

            _ => {
                let res = if let Some(idx) = self.items.get(e) {
                    self.set.get(self.set.find(*idx).unwrap()).unwrap().clone()
                } else {
                    e.clone()
                };
                if e != &res {
                    self.substitute(&res)
                } else {
                    res
                }
            }
        }
    }
}
