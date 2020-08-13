-module(rebar3_cargo_opts).

-export([
    from_state/1,
    mode/1,
    src_dir/1
]).

-export_type([
    t/0
]).

-type mode() :: release | debug | auto.

-record(opts, {
    mode :: mode(),
    src_dir :: string()
}).

-opaque t() :: #opts{}.

-define(DEFAULT_SRC_DIR, ".").

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

    #opts{mode = Mode,
          src_dir = proplists:get_value(src_dir, CargoOpts, ?DEFAULT_SRC_DIR)}.

-spec mode(t()) -> mode().
mode(#opts{mode=Mode}) -> Mode.

-spec src_dir(t()) -> string().
src_dir(#opts{src_dir=SrcDir}) -> SrcDir.

