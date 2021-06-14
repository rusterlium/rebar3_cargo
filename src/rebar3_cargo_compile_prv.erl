-module(rebar3_cargo_compile_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("internal.hrl").

-define(PROVIDER, build).
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
        {example, "rebar3 cargo build"},
        % list of options understood by the plugin
        {opts, []},
        {short_desc, "Compile Rust crates"},
        {desc, "Compile Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    State1 =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_api:info("No current app, using project apps", []),
                NewApps = [
                    rebar3_cargo_compile:compile_app(App, State)
                 || App <- rebar_state:project_apps(State)
                ],
                rebar_state:project_apps(State, NewApps);
            AppInfo ->
                rebar_state:current_app(State, rebar3_cargo_compile:compile_app(AppInfo, State))
        end,

    {ok, State1}.
