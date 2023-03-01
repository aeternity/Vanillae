-module(vw).
-vsn("0.1.0").
-author("Peter Harpending <peter.harpending@gmail.com>").
-copyright("Peter Harpending <peter.harpending@gmail.com>").

-export([start/1]).
-compile([export_all, nowarn_export_all]).


-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    go(ArgV),
    zx:silent_stop().


help_screen() ->
    ["vw: the vanillae wallet\n"
     "USAGE: vw COMMAND\n"
     "\n"
     "COMMANDS\n"
     "  help, --help        Show this menu\n"
     "  decompose tx_...    Decompose a transaction and show its component parts\n"
    ].


go(["help"])                   -> help();
go(["--help"])                 -> help();
go(["decompose", "tx", TxStr]) -> decompose(tx, TxStr);
go(X)                          -> help(), error({invalid_subcommand, X}).




help() ->
    io:format("~ts", [help_screen()]).



decompose(tx, TxStr) ->
    case vd:decompose(TxStr) of
        {ok, X}        -> io:format("~tp~n", [X]);
        {error, Error} -> io:format("ERROR: ~tp~n", [Error])
    end.
