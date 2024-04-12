use std::collections::HashMap;

use crate::{
    program::{Clause, Expr, Program, Task},
    unifier::{Id, Unifier},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    DepthExceeded,
}

#[derive(Debug)]
pub struct Evaluator<'a> {
    prog: &'a Program,
    next_id: usize,
    max_depth: usize,
    stack: Vec<(usize, usize, Unifier, Vec<Id>)>,
}

impl<'a> Iterator for Evaluator<'a> {
    type Item = Result<Unifier, EvalError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((d, next_clause, mut uni, mut cur_task)) = self.stack.pop() {
            if d > self.max_depth {
                return Some(Err(EvalError::DepthExceeded));
            }

            if self.prog.clauses().nth(next_clause + 1).is_some() {
                self.stack
                    .push((d, next_clause + 1, uni.clone(), cur_task.clone()));
            }
            let cur_subtask = cur_task.pop().unwrap();
            let clause = self.prog.clauses().nth(next_clause).unwrap();
            let clause = self.rename_vars(&clause);
            let rule = uni.add_expr(&clause.lhs());

            if uni.unify(cur_subtask, rule) {
                for expr in clause.rhs() {
                    cur_task.push(uni.add_expr(&expr));
                }

                if cur_task.is_empty() {
                    return Some(Ok(uni));
                }
                self.stack.push((d + 1, 0, uni, cur_task));
            }
        }
        None
    }
}

impl Program {
    pub fn eval<'a>(&'a self, task: &Task, max_depth: usize) -> Evaluator<'a> {
        Evaluator::new(self, task, max_depth)
    }
}

impl<'a> Evaluator<'a> {
    fn new(prog: &'a Program, task: &Task, max_depth: usize) -> Self {
        let mut uni = Unifier::default();
        let tasks: Vec<_> = task.exprs().map(|e| uni.add_expr(&e)).collect();
        let stack = vec![(0, 0, uni, tasks)];

        let next_id = prog
            .clauses()
            .map(|c| {
                Self::get_max_id(&c.lhs()).max(
                    c.rhs()
                        .map(|e| Self::get_max_id(&e))
                        .max()
                        .unwrap_or_default(),
                )
            })
            .max()
            .unwrap_or_default()
            .max(
                task.exprs()
                    .map(|e| Self::get_max_id(&e))
                    .max()
                    .unwrap_or_default(),
            )
            + 1;
        Self {
            prog,
            next_id,
            stack,
            max_depth,
        }
    }

    fn get_max_id(e: &Expr) -> usize {
        match e {
            Expr::Var(i) | Expr::Const(i) => *i,
            Expr::Func(i, args) => args
                .iter()
                .map(|e| Self::get_max_id(e))
                .max()
                .unwrap_or_default()
                .max(*i),
        }
    }

    fn rename_vars(&mut self, c: &Clause) -> Clause {
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
}
