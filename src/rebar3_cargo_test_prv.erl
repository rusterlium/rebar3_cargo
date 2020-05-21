-module(rebar3_cargo_test_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("internal.hrl").

-define(PROVIDER, test).
-define(DEPS, [{default, app_discovery}]).

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
            {example, "rebar3 cargo test"},  % How to use the plugin
            {opts, []},                      % list of options understood by the plugin
            {short_desc, "Test Rust crates"},
            {desc, "Test Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    [ test_app(App) || App <- rebar3_cargo_util:get_apps(State) ],
    {ok, State}.


test_app(App) ->
    CrateDirs = rebar3_cargo_util:get_crate_dirs(App),

    %% test individual crates
    [ test_crate(CrateDir) || CrateDir <- CrateDirs ],

    ok.


test_crate(CrateDir) ->
    Cargo = cargo:init(CrateDir),
    {ok, _} = cargo:test_all(Cargo),
    ok.
