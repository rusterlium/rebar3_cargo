-module(rebar3_cargo_util).

-export([
    get_apps/1,
    get_crate_dirs/1,
    get_priv_dir/1
]).

-define(DIR_NAME, "rust_src").


-spec get_apps(rebar_state:t()) -> [rebar_app_info:t()].
get_apps(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end.


-spec get_crate_dirs(rebar_app_info:t()) -> [file:filename_all()].
get_crate_dirs(App) ->
    AppDir = rebar_app_info:dir(App),
    Tomls = filelib:wildcard([?DIR_NAME, "/*/Cargo.toml"], AppDir),
    AbsTomls = [ filename:absname(T, AppDir) || T <- Tomls ],
    [ filename:dirname(T) || T <- AbsTomls ].


-spec get_priv_dir(rebar_app_info:t()) -> file:filename_all().
get_priv_dir(App) ->
    %PrivDir = rebar_app_info:priv_dir(App),  % ensure_dir/1 fails if priv not present (ref https://github.com/erlang/rebar3/issues/1173)
    AppDir = rebar_app_info:dir(App),
    filename:join([AppDir, "priv", "crates"]).
