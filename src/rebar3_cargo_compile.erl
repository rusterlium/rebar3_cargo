-module(rebar3_cargo_compile).

-export([
    force_compile_app/2,
    compile_app/2
]).

-spec force_compile_app(rebar_app_info:t(), rebar_state:t()) -> rebar_app_info:t().
force_compile_app(App, State) ->
    CargoOpts = rebar3_cargo_opts:from_app(App),
    rebar3_cargo_header:ensure(App),

    TargetApp = get_target_app(CargoOpts, App, State),
    do_compile_app(CargoOpts, App, TargetApp, State),
    patch_app(App, TargetApp).

-spec compile_app(rebar_app_info:t(), rebar_state:t()) -> rebar_app_info:t().
compile_app(App, State) ->
    CargoOpts = rebar3_cargo_opts:from_app(App),
    rebar3_cargo_header:ensure(App),

    TargetApp = get_target_app(CargoOpts, App, State),

    case rebar3_cargo_opts:skip(CargoOpts) of
        true ->
            rebar_log:log(info, "Skipping cargo build of ~s", [rebar_app_info:name(App)]);
        _ ->
            do_compile_app(CargoOpts, App, TargetApp, State)
    end,

    patch_app(App, TargetApp).

do_compile_app(CargoOpts, App, TargetApp, State) ->
    IsRelease =
        case rebar3_cargo_opts:mode(CargoOpts) of
            release ->
                true;
            debug ->
                false;
            auto ->
                lists:member(prod, rebar_state:current_profiles(State))
        end,

    Cargo = rebar3_cargo_util:cargo_init(App, CargoOpts, IsRelease),
    Artifacts = cargo:build_all(Cargo),

    lists:foreach(
        fun(Artifact) ->
            do_crate(Artifact, IsRelease, TargetApp)
        end,
        Artifacts
    ).

patch_app(App, TargetApp) ->
    AppName = binary_to_atom(rebar_app_info:name(TargetApp), utf8),
    ErlOpts = [{d, 'CARGO_LOAD_APP', AppName}],

    Opts = rebar_app_info:opts(App),

    ErlOpts1 = ErlOpts ++ rebar_opts:get(Opts, erl_opts, []),
    Opts1 = rebar_opts:set(Opts, erl_opts, ErlOpts1),

    rebar_app_info:opts(App, Opts1).

get_target_app(CargoOpts, App, State) ->
    case rebar3_cargo_opts:load_from_app(CargoOpts) of
        undefined ->
            App;
        Other ->
            [OtherApp | _] = [
                A
             || A <- rebar_state:project_apps(State),
                rebar_app_info:name(A) =:= atom_to_binary(Other, utf8)
            ],
            OtherApp
    end.

do_crate(Artifact, IsRelease, App) ->
    Name = cargo_artifact:name(Artifact),
    _Version = cargo_artifact:version(Artifact),
    Files = cargo_artifact:filenames(Artifact),

    _Type =
        case IsRelease of
            true ->
                "release";
            false ->
                "debug"
        end,

    PrivDir = rebar3_cargo_util:get_priv_dir(App),
    OutDir = filename:join([PrivDir, "crates", Name]),

    filelib:ensure_dir(filename:join([OutDir, "dummy"])),

    % TODO: Distinguish nif vs. other cases here?
    lists:foreach(
        fun(F) ->
            case filelib:is_regular(F) of
                true ->
                    cp(F, OutDir);
                _ ->
                    false
            end
        end,
        Files
    ),

    ok.

-spec cp(file:filename_all(), file:name_all()) -> ok | {error, ignored}.
cp(Src, DstDir) ->
    Extension = filename:extension(Src),
    Fname = filename:basename(Src, Extension),

    % Erlang's load_nif/2 is not prepared to read Mac OS's .dylib extension,
    % take a shortcut here and rename the extension to .so in that case
    Dst0 = maybe_rename_extension(os:type(), Fname, Extension),
    Dst1 = maybe_strip_lib(os:type(), Dst0),

    OutPath = filename:join([DstDir, Dst1]),

    rebar_api:info("  Copying ~s to ~s...", [Fname, OutPath]),

    case file:copy(Src, OutPath) of
        {ok, _} ->
            % Preserve file info
            {ok, SrcFileInfo} = file:read_file_info(Src),
            ok = file:write_file_info(OutPath, SrcFileInfo),
            ok;
        Error ->
            rebar_api:warn("  Failed to copy ~s: ~p", [Fname, Error])
    end,
    ok.

-spec maybe_rename_extension(
    OsType :: {unix | win32, atom()},
    Name :: file:filename_all(),
    Extension :: file:filename_all()
) -> file:filename_all().
maybe_rename_extension({unix, darwin}, Name, <<".dylib">>) ->
    filename:flatten([Name, ".so"]);
maybe_rename_extension(_, Name, Extension) ->
    filename:flatten([Name, Extension]).

maybe_strip_lib({win32, _}, Name) ->
    Name;
maybe_strip_lib(_, Name) ->
    case string:prefix(filename:flatten(Name), "lib") of
        nomatch ->
            filename:flatten(Name);
        Else ->
            Else
    end.
