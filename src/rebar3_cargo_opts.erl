-module(rebar3_cargo_opts).

-export([
    from_state/1,
    mode/1
]).

-export_type([
    t/0
]).

-type mode() :: release | debug | auto.

-record(opts, {
    mode :: mode()
}).

-opaque t() :: #opts{}.

-spec from_state(rebar_state:t()) -> t().
from_state(State) ->
    CargoOpts = rebar_state:get(State, cargo_opts, []),
    Release = proplists:get_value(release, CargoOpts),
    Debug = proplists:get_value(debug, CargoOpts),

    Mode = case {Release, Debug} of
        {true, _} ->
            release;
        {_, true} ->
            debug;
        _ ->
            auto
    end,

    #opts{mode=Mode}.


-spec mode(t()) -> mode().
mode(#opts{mode=Mode}) -> Mode.