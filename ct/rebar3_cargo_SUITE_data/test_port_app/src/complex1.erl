-module(complex1).
-export([start/0,start/1, stop/0, init/1]).
-export([foo/1, bar/1]).

-include("cargo.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    start(filename:join([code:priv_dir(?CARGO_LOAD_APP), "crates", "erl_comm", "erl_comm"])).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
    {complex, Result} ->
        Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
    {call, Caller, Msg} ->
        Port ! {self(), {command, encode(Msg)}},
        receive
        {Port, {data, Data}} ->
            Caller ! {complex, decode(Data)}
        end,
        loop(Port);
    stop ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            exit(normal)
        end;
    {'EXIT', Port, _Reason} ->
        exit(port_terminated)
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

decode([Int]) -> Int.

short_test_() ->
    { setup,
      fun() -> start() end,
      fun(_) -> stop() end,
      [ ?_assertEqual(13, foo(12)),
        ?_assertEqual(24, bar(12)) ]}.
