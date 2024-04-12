use eval::Evaluator;
use parser::{parse, tokenize};
use program::Enviroment;

mod disjoint_set;
mod eval;
mod exprgraph;
mod parser;
mod program;

fn main() {
    // let src = "
    // concat(_, Y, Y) <- ;
    // concat(X.Y, L, X.Z) <- concat(Y, L, Z);
    // rev(_, A, A) <-;
    // rev(X.Y, A, B) <- concat(X._, A, C), rev(Y, C, B);
    // reverse(X, Y) <- rev(X, _, Y);
    // ? reverse(_a._b_._c._, X);
    // ";

    let src = "
    left(X._, R, _B._, X.R) <- ;
    left(X.Y.L, R, Y.L, X.R) <- ;
    right(L, X._, X.L, _B._) <- ;
    right(L, X.Y.R, X.L, Y.R) <- ;
    step(P, L, X.R, Q, L1, R1) <- delta(P, X, Q, Y, _l), left(L, Y.R, L1, R1);
    step(P, L, X.R, Q, L1, R1) <- delta(P, X, Q, Y, _r), right(L, Y.R, L1, R1);
    turingEnd(_, R, R) <-;
    turingEnd(X.L, R, Z) <- turingEnd(L, X.R, Z);
    turing(P, L, R, Z) <- end(P), turingEnd(L, R, Z);
    turing(P, L, R, Z) <- step(P, L, R, Q, L1, R1), turing(Q, L1, R1, Z);

    delta(_q0, _0, _q1, _B, _r) <-;
    delta(_q1, _0, _q1, _0, _r) <-;
    delta(_q2, _0, _q3, _1, _l) <-;
    delta(_q3, _0, _q3, _0, _l) <-;
    delta(_q4, _0, _q4, _0, _l) <-;
    delta(_q5, _0, _q5, _B, _r) <-;
    
    delta(_q0, _1, _q5, _B, _r) <-;
    delta(_q1, _1, _q2, _1, _r) <-;
    delta(_q2, _1, _q2, _1, _r) <-;
    delta(_q3, _1, _q3, _1, _l) <-;
    delta(_q4, _1, _q4, _B, _l) <-;
    delta(_q5, _1, _q5, _B, _r) <-;

    delta(_q2, _B, _q4, _B, _l) <-;
    delta(_q3, _B, _q0, _B, _r) <-;
    delta(_q4, _B, _q6, _0, _r) <-;
    delta(_q5, _B, _q6, _B, _r) <-;
    
    end(_q6) <-;

    ? turing(_q0, _, _0._0._0._0._0._1._0._0._0._, X);
";

    println!("{src}");
    let mut toks = tokenize(src.chars()).unwrap();
    let mut env = Enviroment::new();
    let prog = parse(&mut toks, &mut env).unwrap();

    let mut eval = Evaluator::new(prog, env);
    eval.eval(500).unwrap();
}
