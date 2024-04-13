use crate::{
    parser::TokenStream,
    program::{Enviroment, Expr, Program, Task},
};

use std::io::Write;

#[test]
fn subtract_turing_machine() {
    let src = "
        # Turing machine operation:
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

        # Turing machine description:
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
    ";

    let mut toks = TokenStream::new(src.chars());
    let mut env = Enviroment::new();
    let prog = Program::parse(&mut toks, &mut env).unwrap();

    let (blank_ident, _) = env.get_ident_kind("_B").unwrap();
    let (zero_ident, _) = env.get_ident_kind("_0").unwrap();
    let (nil_ident, _) = env.get_ident_kind("_").unwrap();
    let (dot_ident, _) = env.get_ident_kind(".").unwrap();

    for a in 0..10 {
        for b in 0..10 {
            let task = format!(
                "? turing(_q0, _, {}_1.{}_, X);",
                "_0.".repeat(a as _),
                "_0".repeat(b as _)
            );
            let expect = i32::max(0, a - b);

            for uni in prog.eval(
                &Task::parse(&mut TokenStream::new(task.chars()), &mut env).unwrap(),
                100,
            ) {
                let (x_ident, _) = env.get_ident_kind("X").unwrap();
                let uni = uni.unwrap();

                let x_id = uni.get_expr_id(&Expr::Var(x_ident)).unwrap();

                let res = uni.get_expr(x_id);
                let mut zero_count = 0;
                check_turing_res(
                    &res,
                    zero_ident,
                    blank_ident,
                    nil_ident,
                    dot_ident,
                    &mut zero_count,
                );
                assert_eq!(zero_count, expect as _);
            }
        }
    }
}

fn check_turing_res(
    res: &Expr,
    zero: usize,
    blank: usize,
    nil: usize,
    dot: usize,
    count: &mut usize,
) {
    match res {
        Expr::Const(c) if *c == zero || *c == blank || *c == nil => {
            if *c == zero {
                *count += 1;
            }
        }
        Expr::Func(f, args) if *f == dot => {
            args.iter()
                .for_each(|e| check_turing_res(e, zero, blank, nil, dot, count));
        }
        _ => panic!("Malformed answer: {res:?}"),
    }
}

#[test]
fn list_ops() {
    let src = "
    head(X, X.L) <- ;
    last(X, X._) <- ;
    last(X, Y.L) <- last(X, L);

    prefix(_, L) <-;
    prefix(X.Y, X.Z) <- prefix(Y, Z);

    sublist(X, L) <- prefix(X, L);
    sublist(X, Y.L) <- sublist(X, L);

    concat(_, Y, Y) <- ;
    concat(X.Y, L, X.Z) <- concat(Y, L, Z);
    rev(_, A, A) <-;
    rev(X.Y, A, B) <- rev(Y, X.A, B);
    reverse(X, Y) <- rev(X, _, Y);
    ";
    let mut env = Enviroment::new();
    let prog = Program::parse(&mut TokenStream::new(src.chars()), &mut env).unwrap();

    for (src, expect) in [
        ("? head(X, _);", vec![]),
        ("? head(X, _a._);", vec!["X = _a"]),
        ("? head(X, _a._b._c._);", vec!["X = _a"]),
        ("? head(X, (_a._b._)._c._);", vec!["X = .(_a, .(_b, _))"]),
        ("? head(_a, X._b._c._);", vec!["X = _a"]),
        ("? last(X, _);", vec![]),
        ("? last(X, _a._);", vec!["X = _a"]),
        ("? last(X, _a._b._);", vec!["X = _b"]),
        ("? last(X, _a._b.(_c._d._)._);", vec!["X = .(_c, .(_d, _))"]),
        ("? prefix(X, _a._);", vec!["X = _", "X = .(_a, _)"]),
        (
            "? prefix(X, _a._b._c._);",
            vec![
                "X = _",
                "X = .(_a, _)",
                "X = .(_a, .(_b, _))",
                "X = .(_a, .(_b, .(_c, _)))",
            ],
        ),
        (
            "? prefix(_a._, X._b._c._), prefix(X._, _a._b._c._);",
            vec!["X = _a"],
        ),
        (
            "? sublist(X, _a._b._);",
            vec![
                "X = _",
                "X = .(_a, _)",
                "X = .(_a, .(_b, _))",
                "X = .(_b, _)",
            ],
        ),
        (
            "? concat(_a._b._c._, _d._e._f._, X);",
            vec!["X = .(_a, .(_b, .(_c, .(_d, .(_e, .(_f, _))))))"],
        ),
        (
            "? concat(X, _a._b._c._, _x._a._b._c._);",
            vec!["X = .(_x, _)"],
        ),
        (
            "? reverse(_a._b._c._, X);",
            vec!["X = .(_c, .(_b, .(_a, _)))"],
        ),
    ] {
        println!("{src}");
        let task = Task::parse(&mut TokenStream::new(src.chars()), &mut env).unwrap();
        let mut seen = vec![false; expect.len()];

        for uni in prog.eval(&task, 100) {
            let uni = uni.unwrap();
            let x = Expr::Var(env.get_ident_kind("X").unwrap().0);
            let res = uni.get_expr(uni.get_expr_id(&x).unwrap());

            let mut buf = vec![];
            x.print(&mut buf, &env).unwrap();
            write!(&mut buf, " = ").unwrap();
            res.print(&mut buf, &env).unwrap();

            let res = String::from_utf8(buf).unwrap();
            println!("\t{res}");
            let (i, _) = expect.iter().enumerate().find(|(_, s)| s == &&res).unwrap();
            seen[i] = true;
        }

        assert!(seen.into_iter().all(|t| t))
    }
}
