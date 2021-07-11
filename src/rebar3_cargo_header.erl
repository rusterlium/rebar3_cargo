-module(rebar3_cargo_header).

-export([
    ensure/1
]).

-spec ensure(rebar_app_info:t()) -> ok.
ensure(App) ->
    CurrentVersion = 1,
    Attr = cargo_header_version,

    OutDir = rebar_app_info:dir(App),
    OutPath = filename:join([OutDir, "src", "cargo.hrl"]),
    filelib:ensure_dir(OutPath),

    % Check the currently written version
    ExistingVersion =
        case epp:parse_file(OutPath, []) of
            {ok, Tree} ->
                ARes = erl_syntax_lib:analyze_forms(Tree),
                Attrs = proplists:get_value(attributes, ARes, []),
                proplists:get_value(Attr, Attrs, undefined);
            _Err ->
                undefined
        end,

    if
        ExistingVersion =:= undefined orelse ExistingVersion < CurrentVersion ->
            rebar_log:log(
                info,
                "Writing new cargo header file for ~s...",
                [rebar_app_info:name(App)]
            ),

            AppDefine = "CARGO_LOAD_APP",
            FuncDefine = "CARGO_HRL",

            %% erlfmt-ignore
            Hrl = [
                "-",
                atom_to_list(Attr),
                " ",
                integer_to_list(CurrentVersion),
                ".\n",

                "-ifndef(",
                AppDefine,
                ").\n",
                "-define(",
                AppDefine,
                ",",
                rebar_app_info:name(App),
                ").\n",
                "-endif.\n"
                "-ifndef(",
                FuncDefine,
                ").\n",
                "-define(",
                FuncDefine,
                ", 1).\n",

                "-define(load_nif_from_crate(__CRATE,__INIT),"
                "(fun()->",
                "__APP=?",
                AppDefine,
                ",",
                "__PATH=filename:join([code:priv_dir(__APP),\"crates\",__CRATE,__CRATE]),"
                "erlang:load_nif(__PATH,__INIT)"
                "end)()"
                ").\n",

                "-endif.\n"
            ],

            file:write_file(OutPath, Hrl);
        true ->
            rebar_log:log(debug, "  Skip writing cargo header file", [])
    end.
