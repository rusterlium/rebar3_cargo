-module(rebar3_rust_clean_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(NAMESPACE, rust).
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
            {example, "rebar3 rust clean"},  % How to use the plugin
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
    [ clean_app(App) || App <- rebar3_rust_util:get_apps(State) ],
    {ok, State}.

clean_app(App) ->

    AppDir = rebar_app_info:dir(App),
    PrivDir = filename:join(AppDir, "priv"),

    Tomls = filelib:wildcard("crates/*/Cargo.toml", AppDir),
    AbsTomls = [ filename:absname(T, AppDir) || T <- Tomls ],
    CrateDirs = [ filename:dirname( T) || T <- AbsTomls ],

    %% clean individual crates
    [ clean_crate(CrateDir) || CrateDir <- CrateDirs ],

    %% delete downloaded external crates
    ok = rebar_file_utils:rm_rf(filename:join(AppDir, "extern_crates")),

    %% delete priv/crates
    ok = rebar_file_utils:rm_rf(filename:join(PrivDir, "crates")),

    ok.


clean_crate(CrateDir) ->
    {ok, _} = rebar_utils:sh("cargo clean", [{cd, CrateDir}, {use_stdout, true}]),
    ok.
