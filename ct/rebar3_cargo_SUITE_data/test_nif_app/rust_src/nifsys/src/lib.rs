#[macro_use] extern crate rustler;
#[macro_use] extern crate lazy_static;

use rustler::{Env, Term, NifResult, Encoder};

mod atoms {
    rustler_atoms! {
        atom an_atom;
    }
}

rustler_export_nifs! {
    "nifsys",
    [("static_atom", 0, static_atom),
     ("native_add", 2, native_add),
     ("tuple_add", 1, tuple_add)],
    None
}

fn static_atom<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok(atoms::an_atom().encode(env))
}

/// Add two integers. `native_add(A,B) -> A+B.`
fn native_add<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let num1: i64 = args[0].decode()?;
    let num2: i64 = args[1].decode()?;

    Ok((num1 + num2).encode(env))
}

#[derive(NifTuple)]
struct AddTuple {
    e1: i32,
    e2: i32,
}

/// Add integers provided in a 2-tuple. `tuple_add({A,B}) -> A+B.`
fn tuple_add<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let tuple: AddTuple = args[0].decode()?;

    Ok((tuple.e1 + tuple.e2).encode(env))
}




