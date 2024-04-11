use eval::Evaluator;
use parser::{parse, tokenize};
use program::Enviroment;

mod disjoint_set;
mod eval;
mod parser;
mod program;

fn main() {
    let src = "
path(X, X, _) <-;
path(X, Y, list(list(X, Y), _)) <- edge(X, Y);
path(X, Y, list(list(X, Z), L)) <- edge(X, Z), path(Z, Y, L);

edge(_1, _2) <-;
edge(_1, _3) <-;
edge(_3, _4) <-;
? path(_1, X, P);
";

    let mut toks = tokenize(src.chars()).unwrap();
    let mut env = Enviroment::new();
    let prog = parse(&mut toks, &mut env).unwrap();

    let mut eval = Evaluator::new(prog, env);
    eval.eval(500).unwrap();
}
