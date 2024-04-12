use eval::Evaluator;
use parser::{parse, tokenize};
use program::Enviroment;

mod disjoint_set;
mod eval;
mod exprgraph;
mod parser;
mod program;

fn main() {
    let src = "
    concat(_, Y, Y) <- ;
    concat(X.Y, L, X.Z) <- concat(Y, L, Z);
    rev(_, A, A) <-;
    rev(X.Y, A, B) <- concat(X._, A, C), rev(Y, C, B);
    reverse(X, Y) <- rev(X, _, Y);
    ? reverse(_a._b_._c._, X);
    ";
    println!("{src}");
    let mut toks = tokenize(src.chars()).unwrap();
    let mut env = Enviroment::new();
    let prog = parse(&mut toks, &mut env).unwrap();

    let mut eval = Evaluator::new(prog, env);
    eval.eval(500).unwrap();
}
