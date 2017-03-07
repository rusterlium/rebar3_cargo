-module(rebar3_rust_util).

-export([get_apps/1]).

-include_lib("kernel/include/file.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

get_apps(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end.
