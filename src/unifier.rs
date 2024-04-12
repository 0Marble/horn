use std::{collections::HashMap, rc::Rc};

use crate::{
    disjoint_set::{DisjointSet, SetItem},
    program::{Expr, IdentKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

#[derive(Default, Debug, Clone)]
pub struct Unifier {
    set: DisjointSet<usize>,                        // classes of nodes
    chidren: Vec<Rc<[SetItem]>>,                    // chlidren[node]: classes of children
    parent: HashMap<(usize, Rc<[SetItem]>), usize>, // parent[ident, children]: node
    nodes: Vec<(usize, IdentKind, SetItem)>,        // nodes[node]: (ident, identKind, class)
    leaves: HashMap<usize, usize>,                  // leaves[ident]: node,
}

impl Unifier {
    pub fn get_expr_id(&self, expr: &Expr) -> Option<Id> {
        match expr {
            Expr::Var(ident) | Expr::Const(ident) => self.leaves.get(ident).cloned().map(Id),
            Expr::Func(ident, args) => {
                let mut children = vec![];
                for arg in args.iter() {
                    let child_node = self.get_expr_id(arg)?;
                    let child_class = self.set.find(self.nodes[child_node.0].2).unwrap();
                    children.push(child_class);
                }

                let children: Rc<[SetItem]> = children.into();
                self.parent.get(&(*ident, children)).cloned().map(Id)
            }
        }
    }

    pub fn add_expr(&mut self, expr: &Expr) -> Id {
        match expr {
            Expr::Var(ident) | Expr::Const(ident) => match self.leaves.get(ident) {
                Some(node) => Id(*node),
                None => {
                    let node = self.nodes.len();
                    self.chidren.push(vec![].into());
                    let class = self.set.add_set(node);
                    self.nodes.push((
                        *ident,
                        match expr {
                            Expr::Var(_) => IdentKind::Var,
                            Expr::Const(_) => IdentKind::Const,
                            Expr::Func(_, args) => IdentKind::Func(args.len()),
                        },
                        class,
                    ));
                    self.leaves.insert(*ident, node);
                    Id(node)
                }
            },
            Expr::Func(ident, args) => {
                let mut children = vec![];
                for arg in args.iter() {
                    let child_node = self.add_expr(arg);
                    let child_class = self.set.find(self.nodes[child_node.0].2).unwrap();
                    children.push(child_class);
                }

                let children: Rc<[SetItem]> = children.into();
                let node = *self
                    .parent
                    .entry((*ident, children.clone()))
                    .or_insert_with(|| {
                        let node = self.nodes.len();
                        self.chidren.push(children);
                        let class = self.set.add_set(node);
                        self.nodes.push((
                            *ident,
                            match expr {
                                Expr::Var(_) => IdentKind::Var,
                                Expr::Const(_) => IdentKind::Const,
                                Expr::Func(_, args) => IdentKind::Func(args.len()),
                            },
                            class,
                        ));
                        node
                    });

                Id(node)
            }
        }
    }

    pub fn unify(&mut self, expr1: Id, expr2: Id) -> bool {
        let e1_class = self.nodes[expr1.0].2;
        let e2_class = self.nodes[expr2.0].2;
        let mut uni = Self::default();
        let a = uni.add_expr(&self.get_expr_rec(e1_class));
        let b = uni.add_expr(&self.get_expr_rec(e2_class));
        if !uni.unify_rec(uni.nodes[a.0].2, uni.nodes[b.0].2) {
            return false;
        }

        assert!(self.unify_rec(e1_class, e2_class));
        true
    }

    fn unify_rec(&mut self, expr1: SetItem, expr2: SetItem) -> bool {
        let merge_rule = |a: &usize, b: &usize| -> u8 {
            match (self.nodes[*a].1, self.nodes[*b].1) {
                (IdentKind::Var, IdentKind::Var) => {
                    if self.nodes[*a].0 < self.nodes[*b].0 {
                        1
                    } else {
                        2
                    }
                }
                (IdentKind::Const, IdentKind::Var) => 1,
                (IdentKind::Var, IdentKind::Const) => 2,
                (IdentKind::Var, IdentKind::Func(_)) => 2,
                (IdentKind::Func(_), IdentKind::Var) => 1,
                _ => 0,
            }
        };
        let s = self.set.find(expr1).unwrap();
        let t = self.set.find(expr2).unwrap();

        if s == t {
            return true;
        }

        let s_node = *self.set.get(s).unwrap();
        let t_node = *self.set.get(t).unwrap();
        let (s_ident, s_kind, s_class) = self.nodes[s_node];
        let (t_ident, t_kind, t_class) = self.nodes[t_node];

        match (s_kind, t_kind) {
            (IdentKind::Const, IdentKind::Const) => s_ident == t_ident,
            (IdentKind::Const, IdentKind::Var) => {
                self.set.merge(s_class, t_class, merge_rule);
                true
            }
            (IdentKind::Const, IdentKind::Func(_)) => false,
            (IdentKind::Var, IdentKind::Const) => {
                self.set.merge(s_class, t_class, merge_rule);
                true
            }
            (IdentKind::Var, IdentKind::Var) => {
                self.set.merge(s_class, t_class, merge_rule);
                true
            }
            (IdentKind::Var, IdentKind::Func(_)) => {
                self.set.merge(s_class, t_class, merge_rule);
                true
            }
            (IdentKind::Func(_), IdentKind::Const) => false,
            (IdentKind::Func(_), IdentKind::Var) => {
                self.set.merge(s_class, t_class, merge_rule);
                true
            }
            (IdentKind::Func(_), IdentKind::Func(_)) => {
                if s_ident != t_ident {
                    return false;
                }
                self.set.merge(s_class, t_class, merge_rule);
                self.chidren[s_node]
                    .clone()
                    .into_iter()
                    .zip(self.chidren[t_node].clone().into_iter())
                    .all(|(s, t)| self.unify_rec(*s, *t))
            }
        }
    }

    pub fn get_expr(&self, id: Id) -> Expr {
        self.get_expr_rec(self.nodes[id.0].2)
    }

    fn get_expr_rec(&self, class: SetItem) -> Expr {
        let class = self.set.find(class).unwrap();
        let node = self.set.get(class).unwrap();
        let (ident, kind, _) = self.nodes[*node];

        match kind {
            IdentKind::Const => Expr::Const(ident),
            IdentKind::Var => Expr::Var(ident),
            IdentKind::Func(_) => {
                let mut args = vec![];
                for child_class in self.chidren[*node].iter() {
                    args.push(self.get_expr_rec(*child_class));
                }

                Expr::Func(ident, args.into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn same_expr() {
        let mut uni = Unifier::default();
        let e1 = uni.add_expr(&Expr::Var(0));
        let e2 = uni.add_expr(&Expr::Var(0));
        assert_eq!(e1, e2);

        let mut uni = Unifier::default();
        let e1 = uni.add_expr(&Expr::Const(0));
        let e2 = uni.add_expr(&Expr::Const(0));
        assert_eq!(e1, e2);

        let mut uni = Unifier::default();
        let e1 = uni.add_expr(&Expr::Func(0, vec![Expr::Var(1), Expr::Const(2)].into()));
        let e2 = uni.add_expr(&Expr::Func(0, vec![Expr::Var(1), Expr::Const(2)].into()));
        assert_eq!(e1, e2);

        let mut uni = Unifier::default();
        let e1 = uni.add_expr(&Expr::Func(
            0,
            vec![
                Expr::Func(0, vec![Expr::Var(1), Expr::Var(1)].into()),
                Expr::Const(2),
            ]
            .into(),
        ));
        let e2 = uni.add_expr(&Expr::Func(
            0,
            vec![
                Expr::Func(0, vec![Expr::Var(1), Expr::Var(1)].into()),
                Expr::Const(2),
            ]
            .into(),
        ));
        assert_eq!(e1, e2);
    }

    #[test]
    fn unify() {
        let mut uni = Unifier::default();
        let e1 = uni.add_expr(&Expr::Var(0));
        let e2 = uni.add_expr(&Expr::Const(1));
        assert_ne!(e1, e2);
        assert!(uni.unify(e1, e2));
        assert_eq!(uni.get_expr(e1), Expr::Const(1));
        assert_eq!(uni.get_expr(e2), Expr::Const(1));

        let mut uni = Unifier::default();
        let a = Expr::Var(0);
        let b = Expr::Func(1, vec![Expr::Var(2), Expr::Const(3)].into());
        let c = Expr::Func(1, vec![Expr::Var(2), Expr::Const(3)].into());
        let e1 = uni.add_expr(&a);
        let e2 = uni.add_expr(&b);
        assert_ne!(e1, e2);
        assert!(uni.unify(e1, e2));
        assert_eq!(uni.get_expr(e1), c);
        assert_eq!(uni.get_expr(e2), c);

        let mut uni = Unifier::default();
        let a = Expr::Func(0, vec![Expr::Const(1), Expr::Var(4)].into());
        let b = Expr::Func(0, vec![Expr::Var(3), Expr::Const(2)].into());
        let c = Expr::Func(0, vec![Expr::Const(1), Expr::Const(2)].into());
        let d = Expr::Func(5, vec![Expr::Var(3), Expr::Var(4)].into());
        let e = Expr::Func(5, vec![Expr::Const(1), Expr::Const(2)].into());
        let e1 = uni.add_expr(&a);
        let e2 = uni.add_expr(&b);
        let e3 = uni.add_expr(&d);
        assert_ne!(e1, e2);
        assert!(uni.unify(e1, e2));
        assert_eq!(uni.get_expr(e1), c);
        assert_eq!(uni.get_expr(e2), c);
        let v = uni.add_expr(&Expr::Var(3));
        assert_eq!(uni.get_expr(v), Expr::Const(1));
        let v = uni.add_expr(&Expr::Var(4));
        assert_eq!(uni.get_expr(v), Expr::Const(2));
        assert_eq!(uni.get_expr(e3), e);
    }
}
