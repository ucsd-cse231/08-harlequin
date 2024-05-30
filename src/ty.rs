use std::fmt::{self, Display};

use ess::{span::ByteSpan, Sexp};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyVar(String);

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TyCtor(String);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Ty {
    Int,
    Bool,
    Fun(Vec<Ty>, Box<Ty>),
    Var(TyVar),
    Vec(Box<Ty>, Box<Ty>),
    Ctor(TyCtor, Vec<Ty>),
}

#[derive(Debug, Clone)]
pub struct Poly {
    pub vars: Vec<TyVar>,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Sig<Ann> {
    Infer,
    Assume(Poly, Ann),
    Check(Poly, Ann),
}

impl Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Poly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(
                f,
                "(forall ({}) {})",
                self.vars
                    .iter()
                    .map(|v| v.0.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                self.ty
            )
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::Bool => write!(f, "bool"),
            Ty::Var(x) => write!(f, "{}", x.0),
            Ty::Fun(ts, t) => write!(
                f,
                "(-> ({}) {t})",
                ts.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
            ),
            Ty::Vec(t1, t2) => write!(f, "(vec {t1} {t2})"),
            Ty::Ctor(c, ts) => write!(
                f,
                "({} {})",
                c.0,
                ts.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

pub fn fun(in_tys: Vec<Ty>, out_ty: Ty) -> Ty {
    Ty::Fun(in_tys, Box::new(out_ty))
}

pub fn forall(vars: Vec<TyVar>, ty: Ty) -> Poly {
    Poly { vars, ty }
}

pub fn mono(ty: Ty) -> Poly {
    forall(vec![], ty)
}

pub fn tyv(s: &str) -> Ty {
    Ty::Var(tv(s))
}

pub fn tv(s: &str) -> TyVar {
    TyVar(s.to_string())
}

pub fn tc(s: &str) -> TyCtor {
    TyCtor(s.to_string())
}

pub fn vec(t1: Ty, t2: Ty) -> Ty {
    Ty::Vec(Box::new(t1), Box::new(t2))
}

fn parse_ty(ty: &Sexp) -> Ty {
    match ty {
        Sexp::Sym(op, _) if op == "int" => Ty::Int,
        Sexp::Sym(op, _) if op == "bool" => Ty::Bool,
        Sexp::Sym(a, _) => Ty::Var(tv(a)),
        Sexp::List(ts, _) => match &ts[..] {
            [Sexp::Sym(op, _), Sexp::List(ts, _), t] if op == "->" => {
                let ts = ts.iter().map(parse_ty).collect();
                Ty::Fun(ts, Box::new(parse_ty(t)))
            }
            [Sexp::Sym(op, _), t1, t2] if op == "vec" => vec(parse_ty(t1), parse_ty(t2)),
            [Sexp::Sym(c, _), ts @ ..] /* if is_ctor(c) */ => {
                Ty::Ctor(tc(c), ts.iter().map(parse_ty).collect())
            }
            _ => panic!("parse error: parse_ty"),
        },
        _ => panic!("parse error: parse_ty {ty:?}"),
    }
}

fn parse_poly(pol: &Sexp) -> Poly {
    match pol {
        Sexp::List(xs, _) => match &xs[..] {
            [Sexp::Sym(op, _), Sexp::List(vars, _), ty] if op == "forall" => {
                let vars = vars
                    .iter()
                    .map(|x| match x {
                        Sexp::Sym(x, _) => TyVar(x.to_string()),
                        _ => panic!("parse error"),
                    })
                    .collect();
                forall(vars, parse_ty(ty))
            }
            _ => mono(parse_ty(pol)),
        },
        _ => mono(parse_ty(pol)),
    }
}

pub fn parse_sig(sig: &Sexp) -> Sig<ByteSpan> {
    if let Sexp::List(xs, loc) = sig {
        match &xs[..] {
            [Sexp::Sym(op, _), poly] => {
                let poly = parse_poly(poly);
                if op == "as" {
                    return Sig::Assume(poly, *loc);
                } else if op == "is" {
                    return Sig::Check(poly, *loc);
                } else {
                    panic!("parse error: expected 'as' or 'is' but got {op}")
                }
            }
            _ => panic!("parse error: expected a list of 2 elements"),
        }
    }
    panic!("parse error: expected a list")
}

fn _examples() -> Vec<Poly> {
    vec![
        // (=> (Int Int Int) Int)
        forall(
            vec![],
            Ty::Fun(vec![Ty::Int, Ty::Int, Ty::Int], Box::new(Ty::Int)),
        ),
        // (forall (a) (=> (a) a))
        forall(
            vec![tv("a")],
            Ty::Fun(vec![Ty::Var(tv("a"))], Box::new(Ty::Var(tv("a")))),
        ),
        // (forall (a b) (=> (a b) a))
        forall(
            vec![tv("a"), tv("b")],
            Ty::Fun(
                vec![Ty::Var(tv("a")), Ty::Var(tv("b"))],
                Box::new(Ty::Var(tv("a"))),
            ),
        ),
    ]
}
