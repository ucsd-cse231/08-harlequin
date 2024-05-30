use std::fs::File;
use std::io::prelude::*;
use std::{cmp::max, env};

use error::{Error, Span};
use ess::span::ByteSpan;
use expr::{Arg, Defn, Expr, ExprKind::*, Ident};
use im::{hashmap, HashMap};

pub mod check;
pub mod error;
pub mod expr;
pub mod ty;

type Stack = HashMap<String, i32>;

struct Env {
    stack: Stack,
}

type Res = Result<String, Error>;

impl Env {
    fn empty() -> Env {
        Env { stack: hashmap! {} }
    }

    fn init<A, B>(&self, f: &str, defn: &Defn<A>, free_xs: &HashMap<String, B>) -> Env {
        let mut params = vec![f.to_string()]; // fun-name is the 0-th param
        for param in &defn.params {
            params.push(param.name.clone())
        }
        let mut stack = hashmap! {};
        for (i, param) in params.into_iter().enumerate() {
            let pos = -2 - (i as i32);
            stack.insert(param, pos);
        }
        for (i, (x, _)) in free_xs.iter().enumerate() {
            let pos = 1 + i;
            stack.insert(x.to_string(), pos as i32);
        }
        Env { stack }
    }

    fn get(&self, x: &str) -> Option<&i32> {
        self.stack.get(x)
    }

    fn update(&self, x: String, pos: i32) -> Env {
        let mut stack = self.stack.clone();
        stack.insert(x, pos);
        Env { stack }
    }
}

fn test_number(code: usize) -> String {
    format!(
        "mov rcx, rax
             and rcx, 1
             cmp rcx, 0
             mov rdi, {code}
             jne label_error"
    )
}

fn test_closure(code: usize) -> String {
    format!(
        "mov rcx, rax
         and rcx, 5
         cmp rcx, 5
         mov rdi, {code}
         jne label_error"
    )
}

fn test_arity(arity: usize) -> String {
    format!(
        "mov rcx, rax
         sub rcx, 5         ; remove tag
         mov rcx, [rcx + 8] ; load closure-arity into rcx
         cmp rcx, {arity}   ; compare with actual #args
         mov rdi, 199       ; mismatched args error
         jne label_error"
    )
}

fn label(prefix: String, count: &i32) -> String {
    format!("{prefix}_{count}")
}

const FALSE: usize = 3;
const TRUE: usize = 7;

fn compile_args<A: Span + Clone>(
    exprs: &[Expr<A>],
    env: &Env,
    sp: usize,
    count: &mut i32,
    brk: &str,
    f: &str,
) -> Res {
    let args_code: Vec<_> = exprs
        .iter()
        .enumerate()
        .map(|(i, e)| {
            let e_code = compile_expr(e, env, sp + i, count, brk, false, f)?;
            let e_pos = sp + i;
            Ok(format!(
                "{e_code}
                 mov [rbp - 8*{e_pos}], rax",
            ))
        })
        .collect();
    let args_code: Result<Vec<_>, _> = args_code.into_iter().collect();
    Ok(args_code?.join("\n"))
}

fn tuple_read(tag: usize, index: usize) -> String {
    format!(
        ";; TODO: check rax is pointer
             sub rax, {tag}             ; strip tag
             mov rax, [rax + 8*{index}] ; read at index"
    )
}

fn tuple_alloc(args: &[Arg]) -> String {
    let mut res: Vec<String> = vec![];
    for (i, arg) in args.iter().enumerate() {
        let load_rcx = match arg {
            Arg::Con(n) => format!("mov rcx, {n}"),
            Arg::Var(i_pos) => format!("mov rcx, [rbp - 8*{i_pos}]"),
            Arg::Lbl(label) => format!("lea rcx, QWORD [rel {label}]"),
        };
        res.push(format!(
            "{load_rcx}
             mov [r11 + 8*{i}], rcx",
        ));
    }
    res.push(format!(
        "mov rax, r11
                      add r11, 8*{}",
        args.len()
    ));
    res.join("\n")
}

fn push_args(sp: usize, args: usize) -> String {
    let mut res: Vec<String> = vec![];
    for i in (0..args).rev() {
        let i_pos = sp + i;
        let i_code = format!(
            "mov rcx, [rbp - 8*{i_pos}]
             push rcx",
        );
        res.push(i_code);
    }
    res.join("\n")
}

fn lookup_ident<A: Span>(env: &Env, x: &Ident<A>) -> Result<i32, Error> {
    match env.get(&x.name) {
        Some(x_pos) => Ok(*x_pos),
        None => Err(Error::new(
            x.ann.span(),
            format!("Unbound variable `{}`", x.name),
        )),
    }
}

fn compile_var<A: Span>(env: &Env, x: &Ident<A>) -> Res {
    let x_pos = lookup_ident(env, x)?;
    Ok(format!("mov rax, [rbp - 8*{}]", x_pos))
}

fn compile_expr<A: Span + Clone>(
    e: &Expr<A>,
    env: &Env,
    sp: usize,
    count: &mut i32,
    brk: &str,
    _tr: bool,
    _f: &str,
) -> Res {
    let res = match &e.kind {
        Num(n) => format!("mov rax, {}", n << 1),
        Add1(subexpr) => compile_expr(subexpr, env, sp, count, brk, false, _f)? + "\nadd rax, 2",
        Sub1(subexpr) => compile_expr(subexpr, env, sp, count, brk, false, _f)? + "\nsub rax, 2",
        Neg(subexpr) => compile_expr(subexpr, env, sp, count, brk, false, _f)? + "\nneg rax",
        Var(x) => compile_var(env, x)?,
        Let(x, e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let x_pos = sp;
            let x_save = format!("mov [rbp - 8*{}], rax", x_pos);
            let new_env = env.update(x.name.to_string(), x_pos as i32);
            let e2_code = compile_expr(e2, &new_env, sp + 1, count, brk, _tr, _f)?;
            format!("{e1_code:}\n{x_save:}\n{e2_code:}")
        }
        Plus(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, _f)?;
            let test_code_1 = test_number(99);
            let test_code_2 = test_number(33);

            format!(
                "{e1_code}
                 {test_code_1}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 {test_code_2}
                 add rax, [rbp - 8*{sp}]
                "
            )
        }
        Sub(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, _f)?;
            let test_code_1 = test_number(99);
            let test_code_2 = test_number(33);

            format!(
                "{e1_code}
                 {test_code_1}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 {test_code_2}
                 mov rcx, rax
                 mov rax, [rbp - 8*{sp}]
                 sub rax, rcx"
            )
        }
        Mult(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, _f)?;
            let test_code_1 = test_number(99);
            let test_code_2 = test_number(33);
            let off = 8 * sp;
            format!(
                "{e1_code}
                 {test_code_1}
                 mov [rbp - {off}], rax
                 {e2_code}
                 {test_code_2}
                 sar rax, 1
                 imul rax, [rbp - {off}]
                "
            )
        }
        If(e_cond, e_then, e_else) => {
            *count += 1;
            let e_cond_code = compile_expr(e_cond, env, sp, count, brk, false, _f)?;
            let e_then_code = compile_expr(e_then, env, sp, count, brk, _tr, _f)?;
            let e_else_code = compile_expr(e_else, env, sp, count, brk, _tr, _f)?;
            format!(
                "{e_cond_code}
                      cmp rax, {FALSE}
                      je label_else_{count}
                      {e_then_code}
                      jmp label_exit_{count}
                    label_else_{count}:
                      {e_else_code}
                    label_exit_{count}:"
            )
        }
        Input => "mov rax, [rbp - 8]".to_string(),
        True => {
            format!("mov rax, {TRUE}")
        }
        False => {
            format!("mov rax, {FALSE}")
        }
        Eq(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, _f)?;
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            format!(
                "{e1_code}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 cmp rax, [rbp - 8*{sp}]
                 mov rax, {FALSE}
                 jne {exit}
                 mov rax, {TRUE}
               {exit}:
                "
            )
        }
        Le(e1, e2) => {
            let e1_code = compile_expr(e1, env, sp, count, brk, false, _f)?;
            let e2_code = compile_expr(e2, env, sp + 1, count, brk, false, _f)?;
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            format!(
                "{e1_code}
                 mov [rbp - 8*{sp}], rax
                 {e2_code}
                 cmp rax, [rbp - 8*{sp}]
                 mov rax, {FALSE}
                 jl {exit}
                 mov rax, {TRUE}
               {exit}:
                "
            )
        }
        Set(x, e) => {
            let x_pos = lookup_ident(env, x)?;
            let e_code = compile_expr(e, env, sp, count, brk, false, _f)?;
            format!(
                "{e_code}
                     mov [rbp - 8*{}], rax",
                x_pos
            )
        }
        Block(es) => {
            let n = es.len();
            let e_codes: Vec<_> = es
                .iter()
                .enumerate()
                .map(|(i, e)| compile_expr(e, env, sp, count, brk, _tr && i == n - 1, _f))
                .collect();
            let e_codes: Result<Vec<_>, _> = e_codes.into_iter().collect();
            e_codes?.join("\n")
        }
        Loop(e) => {
            *count += 1;
            let loop_start = label("loop_start".to_string(), count);
            let loop_exit = label("loop_exit".to_string(), count);
            let e_code = compile_expr(e, env, sp, count, &loop_exit, false, _f)?;
            format!(
                "{loop_start}:
                        {e_code}
                        jmp {loop_start}
                     {loop_exit}:"
            )
        }
        Break(e) => {
            let e_code = compile_expr(e, env, sp, count, brk, false, _f)?;
            format!(
                "{e_code}
                     jmp {brk}"
            )
        }
        Print(e) => {
            let e_code = compile_expr(e, env, sp, count, brk, false, _f)?;
            format!(
                "{e_code}
                 mov rdi, rax
                 call snek_print"
            )
        }
        Vek(e1, e2) => {
            let t1 = (**e1).clone();
            let t2 = (**e2).clone();
            let exprs = vec![t1, t2];
            let exprs_code = compile_args(&exprs, env, sp, count, brk, _f)?;
            let args: Vec<Arg> = (sp..sp + exprs.len()).map(Arg::Var).collect();
            let alloc_code = tuple_alloc(&args);
            format!(
                "{exprs_code}
                      {alloc_code}
                      add rax, 0x1"
            )
        }
        Get(e, idx) => {
            let e_code = compile_expr(e, env, sp, count, brk, false, _f)?;
            let tuple_read = tuple_read(1, idx.val());
            format!(
                "{e_code}
                 {tuple_read}",
            )
        }
        Call(f, exprs) => {
            let eval_args = compile_args(exprs, env, sp, count, brk, &f.name)?;
            let push_args = push_args(sp, exprs.len());
            let eval_f = compile_var(env, f)?;
            let pop_args = format!("add rsp, 8*{}", exprs.len());
            let test_closure = test_closure(199);
            let test_arity = test_arity(exprs.len());
            format!(
                "{eval_args}
                 {push_args}
                 {eval_f}
                 {test_closure}
                 {test_arity}
                 push rax
                 sub rax, 5
                 call [rax]
                 {pop_args}"
            )
        }
        Fun(defn) => compile_defn(defn, env, count)?,
    };
    Ok(res)
}

fn compile_exit() -> String {
    "mov rsp, rbp
     pop rbp
     ret"
    .to_string()
}

fn compile_entry<A: Span + Clone>(e: &Expr<A>, sp: usize) -> String {
    let free_vars = free_vars(e);
    let vars = expr_vars(e) + sp + free_vars.len() + 100;
    format!(
        "push rbp
         mov rbp, rsp
         sub rsp, 8*{vars}"
    )
}

fn free_vars_defn<A: Span>(defn: &Defn<A>) -> HashMap<String, ByteSpan> {
    let mut res = free_vars(&defn.body);
    if let Some(name) = &defn.name {
        res.remove(&name.name);
    }
    for param in &defn.params {
        res.remove(&param.name);
    }
    res
}

fn free_vars<A: Span>(e: &Expr<A>) -> HashMap<String, ByteSpan> {
    match &e.kind {
        Num(_) | Input | True | False => HashMap::new(),
        Var(x) => im::hashmap! {x.name.clone() => e.ann.span()},
        Fun(defn) => free_vars_defn(defn),
        Add1(e) | Sub1(e) | Neg(e) | Set(_, e) | Loop(e) | Break(e) | Print(e) | Get(e, _) => {
            free_vars(e)
        }
        Let(x, e1, e2) => free_vars(e1).union(free_vars(e2).without(&x.name)),
        Eq(e1, e2) | Le(e1, e2) | Plus(e1, e2) | Sub(e1, e2) | Mult(e1, e2) | Vek(e1, e2) => {
            free_vars(e1).union(free_vars(e2))
        }
        If(e1, e2, e3) => free_vars(e1).union(free_vars(e2)).union(free_vars(e3)),
        Block(es) => {
            let mut res = HashMap::new();
            for e in es {
                res = res.union(free_vars(e))
            }
            res
        }
        Call(f, es) => {
            let mut res = im::hashmap! { f.name.clone() => f.ann.span() };
            for e in es {
                res = res.union(free_vars(e))
            }
            res
        }
    }
}

fn expr_vars<A>(e: &Expr<A>) -> usize {
    match &e.kind {
        Num(_) | Var(_) | Input | True | False | Fun(_) => 0,
        Add1(e) | Sub1(e) | Neg(e) | Set(_, e) | Loop(e) | Break(e) | Print(e) | Get(e, _) => {
            expr_vars(e)
        }
        Let(_, e1, e2)
        | Eq(e1, e2)
        | Le(e1, e2)
        | Plus(e1, e2)
        | Sub(e1, e2)
        | Mult(e1, e2)
        | Vek(e1, e2) => max(expr_vars(e1), 1 + expr_vars(e2)),
        If(e1, e2, e3) => max(expr_vars(e1), max(expr_vars(e2), expr_vars(e3))),
        Block(es) => es.iter().map(|e| expr_vars(e)).max().unwrap(),
        Call(_, exprs) => exprs
            .iter()
            .enumerate()
            .map(|(i, e)| i + expr_vars(e))
            .max()
            .unwrap_or(1),
    }
}

fn defn_name<A>(def: &Defn<A>, count: &mut i32) -> String {
    match def.name {
        Some(ref name) => name.name.clone(),
        None => {
            *count += 1;
            format!("anon_{count}")
        }
    }
}

fn restore_free_vars<A>(xs: &HashMap<String, A>) -> String {
    let mut res: Vec<String> = vec![];
    for (i, _x) in xs.iter().enumerate() {
        let read_x = tuple_read(5, i + 2);
        let x_pos = i + 1;
        res.push(format!(
            "mov rax, [rbp + 16]         ; load closure into rax
             {read_x}                    ; read x value into rax
             mov [rbp - 8 *{x_pos}], rax ; restore to stack"
        ))
    }
    res.join("\n")
}

fn compile_defn<A: Span + Clone>(defn: &Defn<A>, env: &Env, count: &mut i32) -> Res {
    let f = &defn_name(defn, count);
    let fun_entry = compile_entry(&defn.body, 1);
    let exit_label = format!("fun_exit_{f}");

    let free_xs = free_vars_defn(defn);
    let restore_free_vars = restore_free_vars(&free_xs);
    let body_env = env.init(f, defn, &free_xs);
    let body_code = compile_expr(
        &defn.body,
        &body_env,
        1 + free_xs.len(),
        count,
        &exit_label,
        true,
        f,
    )?;
    let fun_exit = compile_exit();

    let label = format!("fun_start_{f}");
    let mut closure_args = vec![Arg::Lbl(label), Arg::Con(defn.params.len())];
    for (name, ann) in free_xs {
        let x_pos = lookup_ident(env, &Ident { name, ann })?;
        closure_args.push(Arg::Var(x_pos as usize))
    }
    let alloc_closure_tuple = tuple_alloc(&closure_args);

    Ok(format!(
        "jmp fun_finish_{f}
         fun_start_{f}:
           {fun_entry}
         fun_body_{f}:
           {restore_free_vars}
           {body_code}
         fun_exit_{f}:
           {fun_exit}
         fun_finish_{f}:
           {alloc_closure_tuple}
           add rax, 5
        "
    ))
}

fn compile_prog<A: Span + Clone>(prog: &Expr<A>) -> Res {
    check::type_check(prog)?;
    let mut count = 0;
    let e_entry = compile_entry(prog, 1);
    let e_code = compile_expr(
        prog,
        &Env::empty(),
        2,
        &mut count,
        "time_to_exit",
        false,
        "main",
    )?;
    let e_exit = compile_exit();
    Ok(format!(
        "section .text
global our_code_starts_here
extern snek_error
extern snek_print
label_error:
  push rsp
  call snek_error
our_code_starts_here:
 {e_entry}
 mov [rbp - 8], rdi
 mov r11, rsi               ;; save start of heap in r11
 {e_code}
 {e_exit}
time_to_exit:
  ret
"
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // let mut in_file = File::open(in_name)?;
    // let mut in_contents = String::new();
    // in_file.read_to_string(&mut in_contents)?;
    let in_contents = error::read_file(in_name);

    let prog = expr::parse(&in_contents);

    let mut out_file = File::create(out_name)?;

    match compile_prog(&prog) {
        Ok(asm_program) => {
            out_file.write_all(asm_program.as_bytes())?;
            Ok(())
        }
        Err(e) => {
            error::render_errors(in_name, vec![e]);
            panic!("Compilation failed")
        }
    }

    // if let Ok(asm_program) = compile_prog(&prog) {
    //     out_file.write_all(asm_program.as_bytes())?;
    // }

    // Ok(())
}
