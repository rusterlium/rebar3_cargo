-module(rebar3_rust_compile_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, build).
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
    lists:foldl(fun do_app/2, State, get_apps(State)),
    {ok, State}.

-spec get_apps(rebar_state:t()) -> [rebar_app_info:t()].
get_apps(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            rebar_api:info("No current app, using project apps"),
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end.

%% process for one application
do_app(App, State) ->
    IsRelease = lists:member(prod, rebar_state:current_profiles(State)),

    ReleaseFlag =
    case IsRelease of
        true -> " --release";
        false -> ""
    end,

    {ok, MetadataS} =
    rebar_utils:sh(
        lists:flatten(["cargo metadata --format-version=1 --no-deps"]),
        [{use_stdout, false}]
    ),

    Metadata = jsx:decode(list_to_binary(MetadataS), [return_maps]),
    Packages = maps:from_list([
        {maps:get(<<"id">>, M), M}
        || M <- maps:get(<<"packages">>, Metadata)
    ]),

    {ok, Output} =
    rebar_utils:sh(
        lists:flatten(["cargo build --message-format=json-diagnostic-short --quiet", ReleaseFlag]),
        [{env, env()}, {use_stdout, false}]
    ),

    Splitted = string:split(Output, "\n", all),
    Artifacts = lists:foldl(
        fun ("", Artifacts) ->
                Artifacts;

            (Line, Artifacts) ->
                Map = jsx:decode(list_to_binary(Line), [return_maps]),
                #{
                    <<"reason">> := Reason,
                    <<"package_id">> := PackageId
                } = Map,

                case Reason of
                    <<"compiler-artifact">> when is_map_key(PackageId, Packages) ->
                        Artifacts#{
                            PackageId => {
                                maps:get(<<"fresh">>, Map, false), maps:get(<<"filenames">>, Map)
                            }
                        };
                    _ ->
                        Artifacts
                end
        end,
        #{},
        Splitted
    ),

    NifLoadPaths =
    lists:foldl(
        fun (Id, Map) ->
            {Name, Path} =
            do_crate(
                maps:get(Id, Packages),
                maps:get(Id, Artifacts),
                IsRelease,
                App
            ),
            Map#{ Name => Path }
        end,
        #{},
        maps:keys(Artifacts)
    ),

    rebar_api:info("Writing crates header...", []),
    write_header(App, NifLoadPaths),
    % TODO: Prepend ebin/ to include paths
    App.


do_crate(Metadata, {_IsFresh, Files}, IsRelease, App) ->
    #{
        <<"name">> := Name,
        <<"version">> := Version
    } = Metadata,

    Type = case IsRelease of
        true ->
            "release";
        false ->
            "debug"
    end,

    PrivDir = rebar3_rust_util:get_priv_dir(App),
    OutDir = filename:join([PrivDir, Name, Version, Type]),

    filelib:ensure_dir(filename:join([OutDir, "dummy"])),

    rebar_api:info("Copying artifacts for ~s ~s...", [Name, Version]),
    [NifLoadPath] = lists:filtermap(
        fun (F) ->
            case cp(F, OutDir) of
                {ok, NLP} ->
                    {true, NLP};
                _ ->
                    false
            end
        end,
        Files
    ),

    {Name, NifLoadPath}.


env() ->
    case os:type() of
        {unix, darwin} ->
            [{"RUSTFLAGS", "--codegen 'link-args=-flat_namespace -undefined suppress'"}];
        _ ->
            []
    end.


-spec write_header(rebar_app_info:t(), #{ binary() => filename:type() }) ->
    ok.
write_header(App, NifLoadPaths) ->
    Ebin = rebar_app_info:ebin_dir(App),

    Define = "CRATES_HRL",

    Hrl = [
        "-ifndef(", Define, ").\n",
        "-define(", Define, ", 1).\n",
        [
            io_lib:format("-define(crate_~s, ~p).~n", [Name, Path])
            || {Name, Path} <- maps:to_list(NifLoadPaths)
        ],
        "-endif.\n"
    ],

    file:write_file(filename:join(Ebin, "crates.hrl"), Hrl).


cp(Src, Dst) ->
    OsType = os:type(),
    Ext = filename:extension(Src),
    Fname = filename:basename(Src),

    case check_extension(Ext, OsType) of
        true ->
            rebar_api:info("  Copying ~s...", [Fname]),
            OutPath = filename:join([
                Dst,
                filename:basename(Src)
            ]),

            {ok, _} = file:copy(Src, OutPath),

            Len = byte_size(OutPath) - byte_size(Ext),

            NifLoadPath = binary:part(OutPath, 0, Len),
            % rebar_api:info("  Load as erlang:load_nif(~p, 0).", [NifLoadPath]);

            {ok, NifLoadPath};
        _ ->
            rebar_api:debug("  Ignoring ~s", [Fname]),
            {error, ignored}
    end.


check_extension(<<".dll">>, {win32, _}) -> true;
check_extension(<<".dylib">>, {unix, darwin}) -> true;
check_extension(<<".so">>, {unix, Os}) when Os =/= darwin -> true;
check_extension(_, _) -> false.