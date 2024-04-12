use std::collections::HashSet;

use crate::{
    exprgraph::ExprGraph,
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
        let mut uni = ExprGraph::new();
        let tasks: Vec<_> = self.prog.tasks().map(|(_, e)| uni.add_expr(&e)).collect();
        let mut stack = vec![(0, uni, 0, tasks)];

        while let Some((d, mut uni, next_clause, mut task)) = stack.pop() {
            if d > max_depth {
                return Err(EvalError::DepthExceeded);
            }

            if self.prog.clauses().nth(next_clause + 1).is_some() {
                stack.push((d, uni.clone(), next_clause + 1, task.clone()));
            }
            let cur_task = task.pop().unwrap();
            let (_, clause) = self.prog.clauses().nth(next_clause).unwrap();
            let clause = self.env.rename_vars(&clause);
            let rule = uni.add_expr(&clause.lhs());

            if uni.unify(cur_task, rule) {
                for expr in clause.rhs() {
                    task.push(uni.add_expr(&expr));
                }

                if task.is_empty() {
                    self.print_res(&mut uni);
                    println!("=======SUCCESS========");
                } else {
                    stack.push((d + 1, uni, 0, task));
                }
            }
        }

        Ok(())
    }

    fn print_res(&self, uni: &ExprGraph) {
        let mut set = HashSet::new();

        for (_, e) in self.prog.tasks() {
            self.get_vars(&e, &mut set);
        }

        for e in set {
            self.print_expr(&Expr::Var(e));
            print!(" = ");
            let id = uni.get_expr_id(&Expr::Var(e)).unwrap();
            self.print_expr(&uni.get_expr(id));
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
