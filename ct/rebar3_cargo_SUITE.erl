-module(rebar3_cargo_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").


all() -> [test_test_nif_app,
          test_test_port_app,
          test_fails_compile,
          test_fails_test,
          test_release_debug].

test_apps() -> ["test_nif_app",
                "test_port_app",
                "fails_compile",
                "fails_test",
                "release_debug"].


init_per_suite(Config) ->
    #{priv_dir := PrivDir, data_dir := DataDir} = maps:from_list(Config),

    %% Get plugin source directory
    SrcDir = filename:join(lists:takewhile(fun("_build") -> false;
                                              (_) -> true
                                           end, filename:split(DataDir))),

    %% For all test apps...
    lists:foreach(fun(App) ->
                    %% copy from data_dir into priv_dir
                    ok = ec_file:copy(
                        filename:join(DataDir, App),
                        filename:join(PrivDir, App),
                        [recursive]
                    ),

                    RcCheckout = filename:join([PrivDir, App, "_checkouts", "rebar3_cargo"]),
                    ok = filelib:ensure_dir(filename:join(RcCheckout, "dummy")),

                    lists:foreach(
                        fun (Entry) ->
                            ct:pal("Copying from ~s to ~s", [filename:join(SrcDir, Entry), RcCheckout]),
                            ok = ec_file:copy(
                                filename:join(SrcDir, Entry),
                                filename:join(RcCheckout, Entry),
                                [recursive]
                            )
                        end,
                        ["rebar.config", "rebar.lock", "src"]
                    ),

                    ok = ec_file:copy(
                        filename:join(SrcDir, "rebar3"),
                        filename:join([PrivDir, App, "rebar3"])
                    )
                  end,
                  test_apps()),

    Config.

end_per_suite(Config) ->
    Config.

%% The main test application.  Tests Rust port and nif.
test_test_nif_app(Config) ->
    #{priv_dir := PrivDir} = maps:from_list(Config),
    AppDir = filename:join(PrivDir, "test_nif_app"),

    %% check for Rust build artifact
    {ok, _} = rebar_utils:sh("escript rebar3 as prod compile", [{cd, AppDir}, {use_stdout, true}]),
    {ok, _} = rebar_utils:sh("escript rebar3 as prod clean", [{cd, AppDir}, {use_stdout, true}]),
    {ok, _} = rebar_utils:sh("escript rebar3 eunit", [{cd, AppDir}, {use_stdout, true}]),
    {ok, _} = rebar_utils:sh("escript rebar3 clean", [{cd, AppDir}, {use_stdout, true}]),

    ok.

test_test_port_app(Config) ->
    #{priv_dir := PrivDir} = maps:from_list(Config),
    AppDir = filename:join(PrivDir, "test_port_app"),

    ErlCommName = filename:join(
        [AppDir, "priv", "crates", "erl_comm", "0.1.0", "release",
         "erl_comm" ++ case os:type() of {win32, _} -> ".exe"; _ -> "" end
        ]
    ),

    %% check for Rust build artifact
    false = filelib:is_file(ErlCommName),
    {ok, _} = rebar_utils:sh("escript rebar3 as prod compile", [{cd, AppDir}, {use_stdout, true}]),
    true = filelib:is_file(ErlCommName),
    {ok, _} = rebar_utils:sh("escript rebar3 as prod clean", [{cd, AppDir}, {use_stdout, true}]),
    false = filelib:is_file(ErlCommName),
    {ok, _} = rebar_utils:sh("escript rebar3 eunit", [{cd, AppDir}, {use_stdout, true}]),
    {ok, _} = rebar_utils:sh("escript rebar3 clean", [{cd, AppDir}, {use_stdout, true}]),
    false = filelib:is_file(ErlCommName),

    ok.

%% test that rust compile failure causes rebar3 compile failure.
test_fails_compile(Config) ->
    #{priv_dir := PrivDir} = maps:from_list(Config),
    AppDir = filename:join(PrivDir, "fails_compile"),
    {error, _} = rebar_utils:sh("escript rebar3 compile", [{cd, AppDir}, {use_stdout, true}, return_on_error]),
    ok.

%% test that rust test failure causes rebar3 test failure.
test_fails_test(Config) ->
    #{priv_dir := PrivDir} = maps:from_list(Config),
    AppDir = filename:join(PrivDir, "fails_test"),
    {error, _} = rebar_utils:sh("escript rebar3 eunit", [{cd, AppDir}, {use_stdout, true}, return_on_error]),
    ok.

%% check debug vs release builds
test_release_debug(Config) ->
    #{priv_dir := PrivDir} = maps:from_list(Config),
    AppDir = filename:join(PrivDir, "release_debug"),
    Prefix = filename:join([AppDir, "priv", "crates", "build_type", "0.1.0"]),
    DebugExeName = filename:join([Prefix, "debug", "build_type" ++
                                  case os:type() of
                                     {win32, _} -> ".exe";
                                     {unix, _} -> ""
                                  end]),
    {ok, _} = rebar_utils:sh("escript rebar3 compile", [{cd, AppDir}, {use_stdout, true}]),
    {ok, "debug"} = rebar_utils:sh(DebugExeName, [{cd, AppDir}, {use_stdout, true}]),

    ReleaseExeName = filename:join([Prefix, "release", "build_type" ++
                                   case os:type() of
                                      {win32, _} -> ".exe";
                                      {unix, _} -> ""
                                   end]),
    {ok, _} = rebar_utils:sh("rebar3 as prod compile", [{cd, AppDir}, {use_stdout, true}]),
    {ok, "release"} = rebar_utils:sh(ReleaseExeName, [{cd, AppDir}, {use_stdout, true}]),

    ok.
