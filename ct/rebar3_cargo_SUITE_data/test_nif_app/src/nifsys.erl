-module(nifsys).

-include("cargo.hrl").

-export([static_atom/0,native_add/2, tuple_add/1]).
-on_load(init/0).

-include_lib("eunit/include/eunit.hrl").

init() ->
    ?load_nif_from_crate(nifsys, 0).

static_atom() ->
    exit(nif_library_not_loaded).

native_add(_X, _Y) ->
    exit(nif_library_not_loaded).

tuple_add(_X) ->
    exit(nif_library_not_loaded).

short_test_() ->
    [ ?_assertEqual('an_atom', static_atom()),
      ?_assertEqual(7, native_add(3,4)),
      ?_assertEqual(9, tuple_add({4,5}))].
