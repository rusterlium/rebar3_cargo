-module(rebar3_cargo_clean_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("internal.hrl").

-define(PROVIDER, clean).
-define(DEPS, [{default,app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},               % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},               % The module implementation of the task
            {bare, true},                    % The task can be run by the user, always true
            {deps, ?DEPS},                   % The list of dependencies
            {example, "rebar3 cargo clean"},  % How to use the plugin
            {opts, []},                      % list of options understood by the plugin
            {short_desc, "Clean Rust crates"},
            {desc, "Clean Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    [ clean_app(App, State) || App <- rebar3_cargo_util:get_apps(State) ],
    {ok, State}.

clean_app(App, State) ->
    PrivDir = rebar3_cargo_util:get_priv_dir(App),
    CargoOpts = rebar3_cargo_opts:from_state(State),
    Cargo = rebar3_cargo_util:cargo_init(App, CargoOpts, false), 
    catch cargo:clean(Cargo),

    %% delete priv/crates
    ok = rebar_file_utils:rm_rf(filename:join(PrivDir, "crates")),

    ok.