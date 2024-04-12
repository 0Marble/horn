use std::collections::{HashMap, HashSet};

use crate::{
    exprgraph::ExprGraph,
    program::{Clause, Enviroment, Expr, Program, Task},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    DepthExceeded,
}

#[derive(Debug)]
pub struct Evaluator {
    prog: Program,
    next_id: usize,
}

impl Evaluator {
    pub fn new(prog: Program) -> Self {
        Self { prog, next_id: 0 }
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

    pub fn eval(
        &mut self,
        max_depth: usize,
        task: &Task,
        env: &Enviroment,
    ) -> Result<(), EvalError> {
        let mut uni = ExprGraph::new();
        let tasks: Vec<_> = task.exprs().map(|e| uni.add_expr(&e)).collect();
        let mut stack = vec![(0, uni, 0, tasks)];

        self.next_id = self
            .prog
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
            );

        while let Some((d, mut uni, next_clause, mut cur_task)) = stack.pop() {
            if d > max_depth {
                return Err(EvalError::DepthExceeded);
            }

            if self.prog.clauses().nth(next_clause + 1).is_some() {
                stack.push((d, uni.clone(), next_clause + 1, cur_task.clone()));
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
                    self.print_res(task, &mut uni, env);
                    println!("=======SUCCESS========");
                } else {
                    stack.push((d + 1, uni, 0, cur_task));
                }
            }
        }

        Ok(())
    }

    fn print_res(&self, task: &Task, uni: &ExprGraph, env: &Enviroment) {
        let mut set = HashSet::new();

        for e in task.exprs() {
            self.get_vars(&e, &mut set);
        }

        for e in set {
            self.print_expr(&Expr::Var(e), env);
            print!(" = ");
            let id = uni.get_expr_id(&Expr::Var(e)).unwrap();
            self.print_expr(&uni.get_expr(id), env);
            println!();
        }
    }

    fn print_expr(&self, e: &Expr, env: &Enviroment) {
        match e {
            Expr::Var(x) => print!("{}", env.get_ident_name(*x).unwrap()),
            Expr::Const(c) => print!("{}", env.get_ident_name(*c).unwrap()),
            Expr::Func(f, args) => {
                print!("{}(", env.get_ident_name(*f).unwrap());
                self.print_expr(&args[0], env);
                for i in 1..args.len() {
                    print!(", ");
                    self.print_expr(&args[i], env);
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
