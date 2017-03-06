-module(rebar3_rust_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, rust).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},               % The 'user friendly' name of the task
            {module, ?MODULE},               % The module implementation of the task
            {bare, true},                    % The task can be run by the user, always true
            {deps, ?DEPS},                   % The list of dependencies
            {example, "rebar3 compile"},     % How to use the plugin
            {opts, []},                      % list of options understood by the plugin
            {short_desc, "Compile Rust crates"},
            {desc, "Compile Rust crates"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% execute for each app
    [ do_app(App, State) || App <- get_apps(State) ],
    {ok, State}.

get_apps(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end.

%% process for one application
do_app(App, _State) ->
    Opts = rebar_app_info:opts(App),
    PrivDir = rebar_app_info:priv_dir(App),
    AppDir = rebar_app_info:dir(App),



    Tomls = filelib:wildcard("crates/*/Cargo.toml", AppDir),
    AbsTomls = [ filename:absname(T, AppDir) || T <- Tomls ],
    CrateDirs = [ filename:dirname( T) || T <- AbsTomls ],

    [ begin
          OutDir = filename:join([PrivDir, "crates", filename:basename(CrateDir)]),
          do_crate(CrateDir, OutDir, Opts)
      end || CrateDir <- CrateDirs ],
    ok.

do_crate(CrateDir, OutDir, _Opts) ->

    %    Cargo = rebar_utils:find_executable("cargo"), % false or quoted string

    %% get the manifest
    {ok, ManifestIOData} = rebar_utils:sh("cargo read-manifest", [{cd, CrateDir}, {use_stdout, true}]),

    %% extract just the targets that are to be built
    #{targets := Targets0} = jsx:decode(ManifestIOData, [return_maps, attempt_atom]),

    %% and tidy them up
    Targets = [ begin
                    #{name := Name0, kind := Kind0} = Target,
                    Kind = case Kind0 of
                               <<"bin">>    -> bin;
                               <<"dylib">>  -> dylib;
                               <<"cdylib">> -> dylib
                           end,
                    Name = binary_to_list(Name0),
                    #{name := Name, kind := Kind}
                end || Target <- Targets0 ],

    %% and build each target
    [ do_target(Target, CrateDir, OutDir) || Target <- Targets ],

%%    io:format("ManifestIOData: \n~p\n", [ManifestIOData]),
%%    io:format("os_type: ~p\n", [os_type()]),
%%    io:format("OutDir: ~p\n", [OutDir]),

    %filelib:ensure_dir(), % specify dummy filename

    % Don't do this for Rust because it parallelizes builds.  Rust builds can use gobs of RAM.
    % Instead let Cargo parallelize as desired.
    % Furthermore Crates don't neccessarily fit the file translation model this function uses.
    %rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
    ok.


do_target(#{kind := Kind, name := Name}, Target, CrateDir, OutDir) ->
    %% build artifacts individually because some need special args
    Cmd = "cargo rustc --bin" ++ " " ++ linker_args(Kind),
    {ok, _} = rebar_utils:sh(Cmd, [{cd, CrateDir}, {use_stdout, true}]),

    {DstName, SrcName} = target_filenames(Kind, Name),


    ArtifactName =
    TargetName = filename:join(OutDir, Name),

    ok.

-spec(target_filenames(bin|dylib, string()) -> {Dst::string(), Src::string()}).
target_filenames(Kind, Name) -> target_filenames(Kind, Name, os_type()).
target_filenames(bin, Name, win)   -> {Name ++ ".exe", Name ++ ".exe"};
target_filenames(dylib, Name, win) -> {Name ++ ".dll", Name ++ ".dll"};
target_filenames(bin, Name, osx)   -> {Name, Name};
target_filenames(dylib, Name, osx) -> {"lib" ++ Name ++ ".so", "lib" ++ Name ++ ".dylib"};
target_filenames(bin, Name, unix)   -> {Name, Name};
target_filenames(dylib, Name, unix) -> {"lib" ++ Name ++ ".so", "lib" ++ Name ++ ".so"}.

%% OSX needs special args when linking a shared lib for Erlang.
linker_args(Kind) -> linker_args(Kind, os_type()).
linker_args(dylib, osx) -> "-- --codegen 'link-args=-flat_namespace -undefined suppress'";
linker_args(_, _) -> "".


os_type() ->
    case os:type() of
        {win32, nt} -> win;
        {unix, osx} -> osx;  %% FIXME: this is a guess.
        {unix, _} -> unix
    end.

