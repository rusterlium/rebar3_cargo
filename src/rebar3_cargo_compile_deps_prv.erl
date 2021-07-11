-module(rebar3_cargo_compile_deps_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("internal.hrl").

-define(PROVIDER, 'build-all').
-define(DEPS, [{default, lock}]).

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
        {example, "rebar3 cargo build-all"},
        % list of options understood by the plugin
        {opts, []},
        {short_desc, "Compile all dependency Rust crates"},
        {desc, "Compile all dependency Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    AllDeps = rebar_state:all_deps(State),
    AllDeps1 = run_cargo(AllDeps, State),
    State1 = rebar_state:all_deps(State, AllDeps1),

    AllApps = rebar_state:project_apps(State1),
    AllApps1 = run_cargo(AllApps, State1),
    State2 = rebar_state:project_apps(State, AllApps1),

    {ok, State2}.

run_cargo(Apps, State) ->
    lists:map(
        fun(App) ->
            case rebar3_cargo_util:is_cargo_app(App) of
                true ->
                    rebar3_cargo_compile:force_compile_app(App, State);
                false ->
                    App
            end
        end,
        Apps
    ).
