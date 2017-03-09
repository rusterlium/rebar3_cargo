-module(rebar3_rust).

-include_lib("eunit/include/eunit.hrl").

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_rust_compile_prv:init(State0),
    {ok, State2} = rebar3_rust_clean_prv:init(State1),
    {ok, State3} = rebar3_rust_test_prv:init(State2),
    {ok, State3}.
