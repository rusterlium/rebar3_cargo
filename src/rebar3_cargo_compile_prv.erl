-module(rebar3_cargo_compile_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("internal.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROVIDER, build).
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
            {example, "rebar3 rust build"},  % How to use the plugin
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
    State1 =
    case rebar_state:current_app(State) of
        undefined ->
            rebar_api:info("No current app, using project apps", []),
            NewApps =
            lists:foldl(fun do_app/2, State, rebar_state:project_apps(State)),
            rebar_state:project_apps(State, NewApps);
        AppInfo ->
            rebar_state:current_app(State, do_app(AppInfo, State))
    end,

    {ok, State1}.

%% process for one application
-spec do_app(rebar_app_info:t(), rebar_state:t()) -> rebar_app_info:t().
do_app(App, State) ->
    CargoOpts = rebar3_cargo_opts:from_state(State),

    IsRelease = case rebar3_cargo_opts:mode(CargoOpts) of
        release ->
            true;
        debug ->
            false;
        auto ->
            lists:member(prod, rebar_state:current_profiles(State))
    end,

    Cargo = cargo:init(rebar_app_info:dir(App), #{ release => IsRelease }),
    Artifacts = cargo:build_all(Cargo),

    NifLoadPaths =
    lists:foldl(
        fun (Artifact, Map) ->
            {Name, Path} = do_crate(Artifact, IsRelease, App),
            Map#{ Name => Path }
        end,
        #{},
        Artifacts
    ),

    ErlOpts = get_defines(NifLoadPaths),

    Opts = rebar_app_info:opts(App),

    ErlOpts1 = ErlOpts ++ rebar_opts:get(Opts, erl_opts, []),
    Opts1 = rebar_opts:set(Opts, erl_opts, ErlOpts1),

    rebar_api:info("Writing crates header...", []),
    write_header(App, NifLoadPaths),

    rebar_app_info:opts(App, Opts1).


do_crate(Artifact, IsRelease, App) ->
    Name = cargo_artifact:name(Artifact),
    Version = cargo_artifact:version(Artifact),
    Files = cargo_artifact:filenames(Artifact),

    Type = case IsRelease of
        true ->
            "release";
        false ->
            "debug"
    end,

    PrivDir = rebar3_cargo_util:get_priv_dir(App),
    OutDir = filename:join([PrivDir, Name, Version, Type]),
    RelativeLoadPath = filename:join(["crates", Name, Version, Type]),

    filelib:ensure_dir(filename:join([OutDir, "dummy"])),

    rebar_api:info("Copying artifacts for ~s ~s...", [Name, Version]),
    [NifLoadPath] = lists:filtermap(
        fun (F) ->
            case cp(F, OutDir) of
                ok ->
                    Filename = filename:basename(F),
                    case cargo_util:is_dylib(Filename) of
                        true ->
                            {true, filename:rootname(filename:join([RelativeLoadPath, Filename]))};
                        false ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        Files
    ),

    rebar_api:info("Load path ~s", [NifLoadPath]),

    {Name, NifLoadPath}.


-spec write_header(rebar_app_info:t(), #{ binary() => file:filename_all() }) -> ok.
write_header(App, NifLoadPaths) ->
    Define = "CRATES_HRL",
    FuncDefine = "FUNC_CRATES_HRL",

    Hrl = [
        "-ifndef(", Define, ").\n",
        "-define(", Define, ", 1).\n",
        [
            io_lib:format("-define(crate_~s, ~p).~n", [Name, undefined])
            || Name <- maps:keys(NifLoadPaths)
        ],
        "-endif.\n"
        "-ifndef(", FuncDefine, ").\n",
        "-define(", FuncDefine, ", 1).\n",
        "-define(load_nif_from_crate(__APP,__CRATE,__INIT),"
            "(fun()->"
            "__PATH=filename:join(code:priv_dir(__APP),__CRATE),"
            "erlang:load_nif(__PATH,__INIT)"
            "end)()"
        ").\n",
        "-endif.\n"
    ],

    OutDir = rebar_app_info:dir(App),
    OutPath = filename:join([OutDir, "src", "crates.hrl"]),
    filelib:ensure_dir(OutPath),

    file:write_file(OutPath, Hrl).


get_defines(NifLoadPaths) ->
    Opts = [
        get_define(Name, Path) || {Name, Path} <- maps:to_list(NifLoadPaths)
    ],

    [{d, 'CRATES_HRL', 1} | Opts].


get_define(Name, Path) ->
    D = binary_to_atom(
        list_to_binary(io_lib:format("crate_~s", [Name])),
        utf8
    ),

    % TODO: This must be relative to code:priv_dir
    {d, D, binary_to_list(list_to_binary([Path]))}.


-spec cp(file:filename_all(), file:name_all()) -> ok | {error, ignored}.
cp(Src, Dst) ->
    Fname = filename:basename(Src),

    rebar_api:info("  Copying ~s...", [Fname]),
    OutPath = filename:join([
        Dst,
        filename:basename(Src)
    ]),

    {ok, _} = file:copy(Src, OutPath),
    ok.
