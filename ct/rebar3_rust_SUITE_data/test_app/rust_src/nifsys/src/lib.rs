#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;
use std::mem::uninitialized;

/// Create NIF module data and init function.
nif_init!(b"nifsys\0",
          Some(load),
          Some(reload),
          Some(upgrade),
          Some(unload),
          nif!(b"static_atom\0", 0, static_atom),
          nif!(b"native_add\0", 2, native_add, ERL_NIF_DIRTY_JOB_IO_BOUND),
          nif!(b"tuple_add\0", 1, tuple_add, ERL_NIF_DIRTY_JOB_CPU_BOUND));

static mut my_atom: Option<ERL_NIF_TERM> = None;

/// Initialize static atom.
extern "C" fn load(env: *mut ErlNifEnv,
                   _priv_data: *mut *mut c_void,
                   _load_info: ERL_NIF_TERM)
                   -> c_int {
    unsafe {
        my_atom = Some(enif_make_atom(env, b"static atom from Rust\0" as *const u8));
        0
    }
}

/// Does nothing, reports success
extern "C" fn reload(_env: *mut ErlNifEnv,
                     _priv_data: *mut *mut c_void,
                     _load_info: ERL_NIF_TERM)
                     -> c_int {
    0
}

/// Does nothing, reports success
extern "C" fn upgrade(_env: *mut ErlNifEnv,
                      _priv_data: *mut *mut c_void,
                      _old_priv_data: *mut *mut c_void,
                      _load_info: ERL_NIF_TERM)
                      -> c_int {
    0
}

/// Does nothing, reports success
extern "C" fn unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

/// Provide static atom that was initialized by `load()`
extern "C" fn static_atom(_env: *mut ErlNifEnv,
                          _argc: c_int,
                          _args: *const ERL_NIF_TERM)
                          -> ERL_NIF_TERM {
    unsafe { my_atom.unwrap() }
}

/// Add two integers. `native_add(A,B) -> A+B.`
extern "C" fn native_add(env: *mut ErlNifEnv,
                         argc: c_int,
                         args: *const ERL_NIF_TERM)
                         -> ERL_NIF_TERM {
    unsafe {
        let mut a: c_int = uninitialized();
        let mut b: c_int = uninitialized();
        if argc == 2 && 0 != enif_get_int(env, *args, &mut a) &&
           0 != enif_get_int(env, *args.offset(1), &mut b) {
            enif_make_int(env, a + b)
        } else {
            enif_make_badarg(env)
        }
    }
}

/// Add integers provided in a 2-tuple. `tuple_add({A,B}) -> A+B.`
extern "C" fn tuple_add(env: *mut ErlNifEnv,
                        argc: c_int,
                        args: *const ERL_NIF_TERM)
                        -> ERL_NIF_TERM {
    unsafe {
        let mut a: c_int = uninitialized();
        let mut b: c_int = uninitialized();
        let mut size: c_int = uninitialized();
        let mut tup: *const ERL_NIF_TERM = uninitialized();
        if argc == 1 && 0 != enif_get_tuple(env, *args, &mut size, &mut tup) && size == 2 &&
           0 != enif_get_int(env, *tup, &mut a) &&
           0 != enif_get_int(env, *tup.offset(1), &mut b) {
            enif_make_int(env, a + b)
        } else {
            enif_make_badarg(env)
        }
    }
}
