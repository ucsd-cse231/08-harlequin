use im::{hashmap, hashset, HashMap, HashSet};

use crate::{
    error::{Error, Span},
    expr::{Defn, Expr, ExprKind::*, Ident, Index},
    ty::{forall, fun, mono, tv, Poly, Sig, Ty, TyVar},
};

pub fn type_check<A: Span>(e: &Expr<A>) -> Result<(), Error> {
    infer(&TypeEnv::new(), &mut Subst::new(&[]), e)?;
    Ok(())
}

fn infer<A: Span>(env: &TypeEnv, subst: &mut Subst, e: &Expr<A>) -> Result<Ty, Error> {
    let res = match &e.kind {
        Num(_) | Input => Ty::Int,
        True | False => Ty::Bool,
        Add1(e) | Sub1(e) | Neg(e) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::IntToInt),
            &[(**e).clone()],
        )?,
        Plus(e1, e2) | Sub(e1, e2) | Mult(e1, e2) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::IntIntToInt),
            &[(**e1).clone(), (**e2).clone()],
        )?,
        If(e1, e2, e3) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::BoolAAToA),
            &[(**e1).clone(), (**e2).clone(), (**e3).clone()],
        )?,
        Eq(e1, e2) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::AAToBool),
            &[(**e1).clone(), (**e2).clone()],
        )?,
        Le(e1, e2) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::IntIntToBool),
            &[(**e1).clone(), (**e2).clone()],
        )?,
        Set(x, e) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::AAToA),
            &[ident(x), (**e).clone()],
        )?,
        Loop(e) | Break(e) => infer(env, subst, e)?,
        Print(e) => infer_app(&e.ann, env, subst, poly_sig(Prim::AToA), &[(**e).clone()])?,
        Vek(e1, e2) => infer_app(
            &e.ann,
            env,
            subst,
            poly_sig(Prim::ABToVecAB),
            &[(**e1).clone(), (**e2).clone()],
        )?,
        Get(e, idx) => infer_app(&e.ann, env, subst, idx_sig(idx), &[(**e).clone()])?,
        Call(f, es) => {
            let f_sig = env.lookup(f)?;
            infer_app(&e.ann, env, subst, f_sig.clone(), es)?
        }
        Var(x) => {
            let x_sig = env.lookup(x)?;
            subst.instantiate(&x_sig)
        }
        Let(x, e1, e2) => {
            let t1 = infer(env, subst, e1)?;
            let env1 = env.apply(subst);
            let s1 = generalize(&env1, &t1);
            let env2 = env1.extend(&[(x.clone(), s1)]);
            infer(&env2, subst, e2)?
        }
        Fun(defn) => infer_sig(env, subst, defn)?,
        Block(es) => {
            let mut ty = Ty::Int;
            for e in es {
                ty = infer(env, subst, e)?;
            }
            ty
        }
    };
    Ok(res)
}

fn ident<A: Span>(x: &Ident<A>) -> Expr<A> {
    Expr {
        ann: x.ann.clone(),
        kind: Var(x.clone()),
    }
}

// -------------------------------------------------------------------------------------------------
// Type Inference for (Function) Definitions -------------------------------------------------------
// -------------------------------------------------------------------------------------------------

fn infer_sig<A: Span>(env: &TypeEnv, subst: &mut Subst, defn: &Defn<A>) -> Result<Ty, Error> {
    match &defn.sig {
        Sig::Assume(poly, _) => Ok(subst.instantiate(poly)),
        Sig::Infer => infer_defn(env, subst, defn),
        Sig::Check(poly, ann) => check_defn(ann, env, subst, defn, poly),
    }
}

fn check_defn<A: Span>(
    _ann: &A,
    _env: &TypeEnv,
    _subst: &mut Subst,
    _defn: &Defn<A>,
    _expected: &Poly,
) -> Result<Ty, Error> {
    todo!("TBD: fill this in (hard!)")
}

fn infer_defn<A: Span>(_env: &TypeEnv, _subst: &mut Subst, _defn: &Defn<A>) -> Result<Ty, Error> {
    todo!("TBD: fill this in")
}

// ---------------------------------------------------------------------------------------------------
// Type Inference for (Function) Calls   -------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

fn infer_app<A: Span>(
    ann: &A,
    env: &TypeEnv,
    subst: &mut Subst,
    poly: Poly,
    args: &[Expr<A>],
) -> Result<Ty, Error> {
    let mut in_tys = vec![];
    for arg in args {
        in_tys.push(infer(env, subst, arg)?);
    }
    let out = subst.fresh();
    let t2 = Ty::Fun(in_tys, Box::new(out.clone()));
    let mono = subst.instantiate(&poly).apply(subst);
    unify(ann, subst, &mono, &t2)?;
    Ok(out.apply(subst))
}

// ---------------------------------------------------------------------------------------------------
// Unification ---------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------

fn unify<A: Span>(ann: &A, subst: &mut Subst, t1: &Ty, t2: &Ty) -> Result<(), Error> {
    match (t1, t2) {
        (Ty::Int, Ty::Int) | (Ty::Bool, Ty::Bool) => Ok(()),
        (Ty::Fun(ins1, out1), Ty::Fun(ins2, out2)) => {
            unifys(ann, subst, ins1, ins2)?;
            let out1 = out1.apply(subst);
            let out2 = out2.apply(subst);
            unify(ann, subst, &out1, &out2)
        }
        (Ty::Ctor(c1, t1s), Ty::Ctor(c2, t2s)) if *c1 == *c2 => unifys(ann, subst, t1s, t2s),
        (Ty::Vec(s1, s2), Ty::Vec(t1, t2)) => {
            unify(ann, subst, s1, t1)?;
            let s2 = s2.apply(subst);
            let t2 = t2.apply(subst);
            unify(ann, subst, &s2, &t2)
        }
        (Ty::Var(a), t) | (t, Ty::Var(a)) => var_assign(ann, subst, a, t),
        (_, _) => Err(Error::new(
            ann.span(),
            format! {"Type Error: cannot unify {t1} and {t2}"},
        )),
    }
}

fn unifys<A: Span>(ann: &A, subst: &mut Subst, t1s: &[Ty], t2s: &[Ty]) -> Result<(), Error> {
    if t1s.len() != t2s.len() {
        return Err(Error::new(
            ann.span(),
            format!(
                "Type Error: arity mismatch {} and {}",
                t1s.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                t2s.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ));
    }
    for (t1, t2) in t1s.iter().zip(t2s) {
        let t1 = t1.apply(subst);
        let t2 = t2.apply(subst);
        unify(ann, subst, &t1, &t2)?;
    }
    Ok(())
}

fn var_assign<A: Span>(ann: &A, subst: &mut Subst, a: &TyVar, t: &Ty) -> Result<(), Error> {
    if *t == Ty::Var(a.clone()) {
        // assigned to itself, do nothing
        Ok(())
    } else if t.free_vars().contains(a) {
        // oops, fails "occurs" check
        Err(Error::new(
            ann.span(),
            format!("occurs check error: {a} occurs in {t}"),
        ))
    } else {
        // extend `subst` with a |-> t
        subst.extend(a, t);
        Ok(())
    }
}

// --------------------------------------------------------------------------------

fn generalize(env: &TypeEnv, ty: &Ty) -> Poly {
    // 1. compute ty_vars of `ty`
    let ty_vars = ty.free_vars();
    // 2. compute ty_vars of `env`
    let env_vars = env.free_vars();
    // 3. compute unconstrained vars: (1) minus (2)
    let tvs = ty_vars.difference(env_vars).into_iter().collect();
    // 4. slap a `forall` on the unconstrained `tvs`
    forall(tvs, ty.clone())
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
struct TypeEnv(HashMap<String, Poly>);

impl TypeEnv {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn extend<A>(&self, binds: &[(Ident<A>, Poly)]) -> TypeEnv {
        let mut map = self.0.clone();
        for (x, ty) in binds.iter() {
            map.insert(x.name.clone(), ty.clone());
        }
        TypeEnv(map)
    }

    fn lookup<A: Span>(&self, x: &Ident<A>) -> Result<&Poly, Error> {
        match self.0.get(&x.name) {
            Some(t) => Ok(t),
            None => Err(Error::new(
                x.ann.span(),
                format!("unbound variable `{}`", x.name),
            )),
        }
    }
}

#[derive(Debug, Clone)]
struct Subst {
    /// hashmap from type-var := type
    map: HashMap<TyVar, Ty>,
    /// counter used to generate fresh type variables
    idx: usize,
}

impl Subst {
    fn new(tv_tys: &[(TyVar, Ty)]) -> Self {
        let map = tv_tys.iter().cloned().collect();
        Self { map, idx: 0 }
    }

    fn extend(&mut self, tv: &TyVar, ty: &Ty) {
        let subst_tv_ty = Self::new(&[(tv.clone(), ty.clone())]);
        // apply tv |-> ty to all existing mappings
        let mut map = hashmap! {};
        for (k, t) in self.map.iter() {
            map.insert(k.clone(), t.apply(&subst_tv_ty));
        }
        // add new mapping
        map.insert(tv.clone(), ty.clone());
        self.map = map
    }

    fn remove(&mut self, tvs: &[TyVar]) {
        for tv in tvs {
            self.map.remove(tv);
        }
    }

    fn lookup(&self, tv: &TyVar) -> Option<Ty> {
        self.map.get(tv).cloned()
    }

    fn fresh(&mut self) -> Ty {
        let n = self.idx;
        self.idx += 1;
        Ty::Var(tv(&format!("a{n}")))
    }

    fn instantiate(&mut self, poly: &Poly) -> Ty {
        let mut tv_tys = vec![];
        for tv in &poly.vars {
            tv_tys.push((tv.clone(), self.fresh()));
        }
        let su_inst = Subst::new(&tv_tys);
        poly.ty.apply(&su_inst)
    }
}

trait Subable {
    fn apply(&self, subst: &Subst) -> Self;
    fn free_vars(&self) -> HashSet<TyVar>;
}

impl Subable for Ty {
    fn apply(&self, subst: &Subst) -> Self {
        match self {
            Ty::Int => Ty::Int,
            Ty::Bool => Ty::Bool,
            Ty::Var(a) => subst.lookup(a).unwrap_or(Ty::Var(a.clone())),
            Ty::Fun(in_tys, out_ty) => {
                let in_tys = in_tys.iter().map(|ty| ty.apply(subst)).collect();
                let out_ty = out_ty.apply(subst);
                fun(in_tys, out_ty)
            }
            Ty::Vec(ty0, ty1) => {
                let ty0 = ty0.apply(subst);
                let ty1 = ty1.apply(subst);
                Ty::Vec(Box::new(ty0), Box::new(ty1))
            }
            Ty::Ctor(c, tys) => {
                let tys = tys.iter().map(|ty| ty.apply(subst)).collect();
                Ty::Ctor(c.clone(), tys)
            }
        }
    }

    fn free_vars(&self) -> HashSet<TyVar> {
        match self {
            Ty::Int | Ty::Bool => hashset! {},
            Ty::Var(a) => hashset! {a.clone()},
            Ty::Fun(in_tys, out_ty) => free_vars_many(in_tys).union(out_ty.free_vars()),
            Ty::Vec(t0, t1) => t0.free_vars().union(t1.free_vars()),
            Ty::Ctor(_, tys) => free_vars_many(tys),
        }
    }
}

fn free_vars_many(tys: &[Ty]) -> HashSet<TyVar> {
    tys.iter()
        .map(|ty| ty.free_vars())
        .fold(HashSet::new(), |acc, x| acc.union(x))
}

impl Subable for Poly {
    fn apply(&self, subst: &Subst) -> Self {
        let mut subst = subst.clone();
        subst.remove(&self.vars);
        forall(self.vars.clone(), self.ty.apply(&subst))
    }

    fn free_vars(&self) -> HashSet<TyVar> {
        let bound_vars = self.vars.clone().into();
        self.ty.free_vars().difference(bound_vars)
    }
}

impl Subable for TypeEnv {
    fn apply(&self, subst: &Subst) -> Self {
        let mut map = HashMap::new();
        for (name, poly) in self.0.iter() {
            map.insert(name.clone(), poly.apply(subst));
        }
        TypeEnv(map)
    }

    fn free_vars(&self) -> HashSet<TyVar> {
        let mut res = HashSet::new();
        for poly in self.0.values() {
            res = res.union(poly.free_vars());
        }
        res
    }
}

// ----------------------------------------------------------------------------------------------------

enum Prim {
    IntToInt,
    IntIntToInt,
    IntIntToBool,
    AToA,
    AAToBool,
    BoolAAToA,
    AAToA,
    ABToVecAB,
}

fn poly_sig(op: Prim) -> Poly {
    match op {
        Prim::IntToInt => mono(Ty::Fun(vec![Ty::Int], Box::new(Ty::Int))),
        Prim::IntIntToInt => mono(Ty::Fun(vec![Ty::Int, Ty::Int], Box::new(Ty::Int))),
        Prim::IntIntToBool => mono(Ty::Fun(vec![Ty::Int, Ty::Int], Box::new(Ty::Bool))),
        _ => todo!("TBD: fill this in"),
    }
}

fn idx_sig(idx: &Index) -> Poly {
    match idx {
        _ => todo!("TBD: fill this in"),
    }
}

#[cfg(test)]
mod tests {
    use crate::ty::tyv;

    use super::*;

    #[test]
    fn test_subst() {
        use crate::ty::tyv;

        let ex_subst = Subst::new(&[
            (tv("a"), Ty::Int),
            (tv("b"), Ty::Bool),
            (tv("c"), fun(vec![Ty::Int, Ty::Int], Ty::Int)),
        ]);

        let ty1 = fun(vec![tyv("a"), tyv("z")], tyv("b"));

        let ty2 = ty1.apply(&ex_subst);

        assert!(ty2 == fun(vec![Ty::Int, tyv("z")], Ty::Bool));
    }

    #[test]
    fn unify0() {
        let mut subst = Subst::new(&[]);
        let _ = unify(&(0, 0), &mut subst, &Ty::Int, &Ty::Int);
        assert!(format!("{:?}", subst) == "Subst { map: {}, idx: 0 }")
    }

    #[test]
    fn unify1() {
        let mut subst = Subst::new(&[]);
        let t1 = fun(vec![tyv("a")], Ty::Int);
        let t2 = fun(vec![Ty::Bool], tyv("b"));
        let _ = unify(&(0, 0), &mut subst, &t1, &t2);
        assert!(subst.map == hashmap! {tv("a") => Ty::Bool, tv("b") => Ty::Int})
    }

    #[test]
    fn unify2() {
        let mut subst = Subst::new(&[]);
        let t1 = tyv("a");
        let t2 = fun(vec![tyv("a")], Ty::Int);
        let res = unify(&(0, 0), &mut subst, &t1, &t2).err().unwrap();
        assert!(format!("{res}") == "occurs check error: a occurs in (-> (a) int)")
    }

    #[test]
    fn unify3() {
        let mut subst = Subst::new(&[]);
        let res = unify(&(0, 0), &mut subst, &Ty::Int, &Ty::Bool)
            .err()
            .unwrap();
        assert!(format!("{res}") == "Type Error: cannot unify int and bool")
    }

    #[test]
    fn inst() {
        let t_id = forall(vec![tv("a")], fun(vec![tyv("a")], tyv("a")));
        let mut subst = Subst::new(&[]);
        let ty0 = subst.instantiate(&t_id);
        let ty1 = subst.instantiate(&t_id);
        let ty2 = subst.instantiate(&t_id);
        assert!(ty0 == fun(vec![tyv("a0")], tyv("a0")));
        assert!(ty1 == fun(vec![tyv("a1")], tyv("a1")));
        assert!(ty2 == fun(vec![tyv("a2")], tyv("a2")));
    }
}
