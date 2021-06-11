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
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 cargo test"},
        % list of options understood by the plugin
        {opts, []},
        {short_desc, "Test Rust crates"},
        {desc, "Test Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    [test_app(App, State) || App <- rebar3_cargo_util:get_apps(State)],
    {ok, State}.

test_app(App, State) ->
    CargoOpts = rebar3_cargo_opts:from_state(State),
    Cargo = rebar3_cargo_util:cargo_init(App, CargoOpts, false),
    cargo:test_all(Cargo).
