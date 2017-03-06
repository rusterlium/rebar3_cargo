-module(rebar3_rust).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_rust_prv:init(State),
    {ok, State1}.
