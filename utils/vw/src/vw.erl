-module(vw).
-vsn("0.1.0").
-author("Peter Harpending <ceverett@tsuriai.jp>").
-copyright("Peter Harpending <ceverett@tsuriai.jp>").

-export([start/1]).
-compile([export_all, nowarn_export_all]).

-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    go(ArgV),
    zx:silent_stop().

%% Taking break
%%
%% WHEN back: get decompose to work

go(["help"]) ->
    help();
go(["--help"]) ->
    help();
go(["generate", "keypair"]) ->
    error(nyi);
go(["decompose", TxStr]) ->
    decompose(TxStr);
go(_) ->
    error(invalid_subcommand).

help() ->
    io:format("you can't help people who won't help themselves~n", []).

decompose(TxStr) ->
    case vd:decompose(TxStr) of
        {ok, X} ->
            io:format("~tp~n", [X]);
        {error, Error} ->
            io:format("ERROR: ~tp~n", [Error])
    end.
