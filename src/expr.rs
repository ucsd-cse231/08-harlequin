use core::panic;
use std::fmt::{self, Display};

use ess::{span::ByteSpan, *};

use crate::ty::{self, Sig};

#[derive(Debug, Clone)]
pub struct Defn<Ann> {
    pub name: Option<Ident<Ann>>,
    pub params: Vec<Ident<Ann>>,
    pub body: Box<Expr<Ann>>,
    pub sig: Sig<Ann>,
}

#[derive(Debug, Clone)]
pub struct Expr<Ann> {
    pub ann: Ann,
    pub kind: ExprKind<Ann>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<Ann> {
    Num(i32),
    Add1(Box<Expr<Ann>>),
    Sub1(Box<Expr<Ann>>),
    Neg(Box<Expr<Ann>>),
    Var(Ident<Ann>),
    Let(Ident<Ann>, Box<Expr<Ann>>, Box<Expr<Ann>>),
    Plus(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Sub(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Mult(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Input,
    True,
    False,
    If(Box<Expr<Ann>>, Box<Expr<Ann>>, Box<Expr<Ann>>),
    Eq(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Le(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Set(Ident<Ann>, Box<Expr<Ann>>),
    Block(Vec<Expr<Ann>>),
    Loop(Box<Expr<Ann>>),
    Break(Box<Expr<Ann>>),
    Print(Box<Expr<Ann>>),
    Call(Ident<Ann>, Vec<Expr<Ann>>),
    Vek(Box<Expr<Ann>>, Box<Expr<Ann>>),
    Get(Box<Expr<Ann>>, Index),
    Fun(Defn<Ann>),
}

#[derive(Debug, Clone)]
pub struct Ident<Ann> {
    pub name: String,
    pub ann: Ann,
}

#[derive(Debug, Clone)]
pub enum Index {
    Zero,
    One,
}

impl Index {
    pub fn val(&self) -> usize {
        match self {
            Index::Zero => 0,
            Index::One => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Arg {
    Var(usize),  // variable on stack frame at rbp - 8*i
    Lbl(String), // code label
    Con(usize),  // constant
}

fn num<Ann>(n: i32, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Num(n),
        ann,
    }
}

fn add1<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Add1(Box::new(e)),
        ann,
    }
}

fn sub1<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Sub1(Box::new(e)),
        ann,
    }
}

fn negate<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Neg(Box::new(e)),
        ann,
    }
}

fn loope<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Loop(Box::new(e)),
        ann,
    }
}

fn printe<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Print(Box::new(e)),
        ann,
    }
}

fn breake<Ann>(e: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Break(Box::new(e)),
        ann,
    }
}

fn plus<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Plus(Box::new(e1), Box::new(e2)),
        ann,
    }
}

fn sub<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Sub(Box::new(e1), Box::new(e2)),
        ann,
    }
}
fn mult<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Mult(Box::new(e1), Box::new(e2)),
        ann,
    }
}

pub fn eq<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Eq(Box::new(e1), Box::new(e2)),
        ann,
    }
}

pub fn le<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Le(Box::new(e1), Box::new(e2)),
        ann,
    }
}

fn ite<Ann>(e1: Expr<Ann>, e2: Expr<Ann>, e3: Expr<Ann>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::If(Box::new(e1), Box::new(e2), Box::new(e3)),
        ann,
    }
}

fn fun<Ann>(
    name: Option<Ident<Ann>>,
    params: Vec<Ident<Ann>>,
    body: Box<Expr<Ann>>,
    sig: Sig<Ann>,
    ann: Ann,
) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Fun(Defn {
            name,
            params,
            body,
            sig,
        }),
        ann,
    }
}

fn lete<Ann>(x: Ident<Ann>, e1: Box<Expr<Ann>>, e2: Box<Expr<Ann>>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Let(x, e1, e2),
        ann,
    }
}

fn set<Ann>(x: Ident<Ann>, e: Box<Expr<Ann>>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Set(x, e),
        ann,
    }
}

fn block<Ann>(es: Vec<Expr<Ann>>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Block(es),
        ann,
    }
}

fn vek<Ann>(e1: Box<Expr<Ann>>, e2: Box<Expr<Ann>>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Vek(e1, e2),
        ann,
    }
}

fn get<Ann>(e1: Box<Expr<Ann>>, i: Index, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Get(e1, i),
        ann,
    }
}

fn call<Ann>(f: Ident<Ann>, es: Vec<Expr<Ann>>, ann: Ann) -> Expr<Ann> {
    Expr {
        kind: ExprKind::Call(f, es),
        ann,
    }
}

type ExprS = Expr<ByteSpan>;
type BindS = Ident<ByteSpan>;
type IdentS = Ident<ByteSpan>;

pub fn parse(s: &str) -> ExprS {
    // let s = format!("({})", s);
    let (exprs, errs) = ess::parse(s);
    if let Some(err) = errs {
        panic!("parse error! {:?}", err)
    };

    if let [defs @ .., expr] = &exprs[..] {
        let defs = defs.iter().map(|e| parse_defn(e)).collect();
        prog(defs, parse_expr(expr))
    } else {
        panic!("syntax error: program must contain a main expression")
    }
}

fn parse_bind(s: &Sexp) -> (BindS, ExprS) {
    match s {
        Sexp::List(vec, _) => match &vec[..] {
            [Sexp::Sym(x, ann), e] => {
                let xb = Ident {
                    name: x.to_string(),
                    ann: *ann,
                };
                (xb, parse_expr(e))
            }
            _ => panic!("parse error"),
        },
        _ => panic!("parse error"),
    }
}

fn parse_ident(s: &Sexp) -> IdentS {
    match s {
        Sexp::Sym(x, ann) => Ident {
            name: x.to_string(),
            ann: *ann,
        },
        _ => panic!("parse error"),
    }
}

pub fn parse_defn(s: &Sexp) -> ExprS {
    let Sexp::List(es, ann) = s else {
        panic!("syntax error: expected a list")
    };
    match &es[..] {
        [Sexp::Sym(op, _), Sexp::List(xs, _), sig, body] if op == "defn" => {
            let [name, params @ ..] = &xs[..] else {
                panic!("missing function name");
            };
            let sig = ty::parse_sig(sig);
            let body = Box::new(parse_expr(body));
            let name = parse_ident(name);
            let params = params.iter().map(parse_ident).collect();
            fun(Some(name), params, body, sig, *ann)
        }

        [Sexp::Sym(op, _), Sexp::List(xs, _), body] if op == "defn" => {
            let [name, params @ ..] = &xs[..] else {
                panic!("missing function name");
            };
            let body = Box::new(parse_expr(body));
            let name = parse_ident(name);
            let params = params.iter().map(parse_ident).collect();
            fun(Some(name), params, body, Sig::Infer, *ann)
        }
        [Sexp::Sym(op, _), Sexp::List(xs, _), body] if op == "fn" => {
            let body = Box::new(parse_expr(body));
            let params = xs.iter().map(parse_ident).collect();
            fun(None, params, body, Sig::Infer, *ann)
        }

        _ => panic!("syntax error: expected a list of 4 elements: {s:?}"),
    }
}

fn prog(defs: Vec<ExprS>, expr: ExprS) -> ExprS {
    let mut res = expr;
    for def in defs.into_iter().rev() {
        if let ExprKind::Fun(Defn { name, .. }) = &def.kind {
            if let Some(x) = name {
                let ann = def.ann;
                res = lete(x.clone(), Box::new(def), Box::new(res), ann);
            }
        } else {
            panic!("syntax error: expected a function definition")
        }
    }
    res
}

fn parse_index(s: &Sexp) -> Index {
    match s {
        Sexp::Int(0, _) => Index::Zero,
        Sexp::Int(1, _) => Index::One,
        _ => panic!("parse error: {s:?}"),
    }
}

fn ident(s: String, loc: ByteSpan) -> IdentS {
    Ident {
        name: s.to_string(),
        ann: loc,
    }
}

pub fn var(s: String, loc: ByteSpan) -> ExprS {
    Expr {
        kind: ExprKind::Var(ident(s, loc)),
        ann: loc,
    }
}

fn parse_expr(s: &Sexp) -> ExprS {
    match s {
        Sexp::Int(n, loc) => num(i32::try_from(*n).unwrap(), *loc),

        Sexp::Sym(s, loc) if s == "input" => Expr {
            kind: ExprKind::Input,
            ann: *loc,
        },
        Sexp::Sym(s, loc) if s == "true" => Expr {
            kind: ExprKind::True,
            ann: *loc,
        },
        Sexp::Sym(s, loc) if s == "false" => Expr {
            kind: ExprKind::False,
            ann: *loc,
        },

        Sexp::Sym(s, loc) => var(s.to_string(), *loc),

        Sexp::List(vec, ann) => match &vec[..] {
            [Sexp::Sym(op, _), e] if op == "add1" => add1(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e] if op == "sub1" => sub1(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e] if op == "negate" => negate(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e] if op == "loop" => loope(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e] if op == "print" => printe(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e] if op == "break" => breake(parse_expr(e), *ann),
            [Sexp::Sym(op, _), e1, e2] if op == "+" => plus(parse_expr(e1), parse_expr(e2), *ann),
            [Sexp::Sym(op, _), e1, e2] if op == "-" => sub(parse_expr(e1), parse_expr(e2), *ann),
            [Sexp::Sym(op, _), e1, e2] if op == "*" => mult(parse_expr(e1), parse_expr(e2), *ann),
            [Sexp::Sym(op, _), e1, e2] if op == "=" => eq(parse_expr(e1), parse_expr(e2), *ann),
            [Sexp::Sym(op, _), e1, e2] if op == "<=" => le(parse_expr(e1), parse_expr(e2), *ann),
            [Sexp::Sym(op, _), e1, e2, e3] if op == "if" => {
                ite(parse_expr(e1), parse_expr(e2), parse_expr(e3), *ann)
            }
            [Sexp::Sym(op, _), bind, e2] if op == "let" => {
                let (x, e1) = parse_bind(bind);
                let e2 = parse_expr(e2);
                lete(x, Box::new(e1), Box::new(e2), *ann)
            }
            [Sexp::Sym(op, _), Sexp::List(binds, _), e] if op == "let*" => {
                let xes: Vec<_> = binds.iter().map(parse_bind).collect();
                let mut res = parse_expr(e);
                for (x, e) in xes.into_iter().rev() {
                    res = lete(x, Box::new(e), Box::new(res), *ann)
                }
                res
            }

            [Sexp::Sym(op, _), x, e] if op == "set!" => {
                set(parse_ident(x), Box::new(parse_expr(e)), *ann)
            }
            [Sexp::Sym(op, _), exprs @ ..] if op == "block" => {
                block(exprs.iter().map(parse_expr).collect(), *ann)
            }
            [Sexp::Sym(op, _), e1, e2] if op == "vec" => {
                vek(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), *ann)
            }
            [Sexp::Sym(op, _), e1, e2] if op == "vec-get" => {
                get(Box::new(parse_expr(e1)), parse_index(e2), *ann)
            }
            [Sexp::Sym(op, _), _, _] if op == "defn" || op == "fn" => parse_defn(s),
            [f, exprs @ ..] => call(parse_ident(f), exprs.iter().map(parse_expr).collect(), *ann),
            _ => panic!("parse error (1) {:?}", s),
        },
        _ => panic!("parse error (2) {:?}", s),
    }
}

impl<A> Display for Expr<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExprKind::Num(n) => write!(f, "{n}"),
            ExprKind::Add1(e) => write!(f, "(add1 {e})"),
            ExprKind::Sub1(e) => write!(f, "(sub1 {e})"),
            ExprKind::Neg(e) => write!(f, "(- {e})"),
            ExprKind::Var(x) => write!(f, "{}", x.name),
            ExprKind::Let(x, e1, e2) => write!(f, "(let ({} {}) {})", x.name, e1, e2),
            ExprKind::Plus(e1, e2) => write!(f, "(+ {} {})", e1, e2),
            ExprKind::Sub(e1, e2) => write!(f, "(- {} {})", e1, e2),
            ExprKind::Mult(e1, e2) => write!(f, "(* {} {})", e1, e2),
            ExprKind::Input => write!(f, "input"),
            ExprKind::True => write!(f, "true"),
            ExprKind::False => write!(f, "false"),
            ExprKind::If(e1, e2, e3) => write!(f, "(if {} {} {})", e1, e2, e3),
            ExprKind::Eq(e1, e2) => write!(f, "(= {} {})", e1, e2),
            ExprKind::Le(e1, e2) => write!(f, "(<= {} {})", e1, e2),
            ExprKind::Set(x, e) => write!(f, "(set! {} {})", x.name, e),
            ExprKind::Block(e) => write!(
                f,
                "(block {})",
                e.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            ExprKind::Loop(e) => write!(f, "(loop {})", e),
            ExprKind::Break(e) => write!(f, "(break {})", e),
            ExprKind::Print(e) => write!(f, "(print {})", e),
            ExprKind::Call(g, es) => write!(
                f,
                "({} {})",
                g.name,
                es.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            ExprKind::Vek(e1, e2) => write!(f, "(vec {} {})", e1, e2),
            ExprKind::Get(e1, idx) => write!(f, "(vec-get {} {})", e1, idx.val()),
            ExprKind::Fun(defn) => write!(f, "{}", defn),
        }
    }
}

impl<A> Display for Defn<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(
                f,
                "(defn {} ({}) {})",
                name.name,
                self.params
                    .iter()
                    .map(|x| x.name.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                self.body
            ),
            None => write!(
                f,
                "(fn ({}) {})",
                self.params
                    .iter()
                    .map(|x| x.name.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                self.body
            ),
        }
    }
}
