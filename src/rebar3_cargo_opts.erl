-module(rebar3_cargo_opts).

-export([
    from_app/1,
    from_state/1
]).

-export([
    mode/1,
    skip/1,
    load_from_app/1,
    src_dir/1
]).

-export_type([
    t/0
]).

-define(DEFAULT_SRC_DIR, ".").
-define(KEY, cargo_opts).

-type mode() :: release | debug | auto.

-record(opts, {
    mode :: mode(),
    skip :: boolean(),
    load_from_app :: atom(),
    src_dir :: filelib:file_path()
}).

-opaque t() :: #opts{}.

-spec from_app(rebar_app_info:t()) -> t().
from_app(App) ->
    AppOpts = rebar_app_info:opts(App),
    Opts =
        case dict:find(?KEY, AppOpts) of
            {ok, O} ->
                O;
            _ ->
                []
        end,

    from_opts(Opts).

-spec from_state(rebar_state:t()) -> t().
from_state(State) ->
    CargoOpts = rebar_state:get(State, cargo_opts, []),
    from_opts(CargoOpts).

from_opts(Opts) ->
    Release = proplists:get_value(release, Opts),
    Debug = proplists:get_value(debug, Opts),

    Mode =
        case {Release, Debug} of
            {true, _} ->
                release;
            {_, true} ->
                debug;
            _ ->
                auto
        end,

    #opts{
        mode = Mode,
        skip = proplists:get_bool(skip, Opts),
        load_from_app = proplists:get_value(load_from_app, Opts),
        src_dir = proplists:get_value(src_dir, Opts, ?DEFAULT_SRC_DIR)
    }.

-spec mode(t()) -> mode().
mode(#opts{mode = Mode}) -> Mode.

-spec skip(t()) -> boolean().
skip(#opts{skip = Skip}) -> Skip.

-spec load_from_app(t()) -> atom().
load_from_app(#opts{load_from_app = App}) -> App.

-spec src_dir(t()) -> string().
src_dir(#opts{src_dir = SrcDir}) -> SrcDir.
