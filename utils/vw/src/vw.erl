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

go(["help"])                -> help();
go(["--help"])              -> help();
go(["decompose", TxStr])    -> decompose(TxStr);
go(["generate", "keypair"]) -> generate_keypair();
go(X)                       -> error({invalid_subcommand, X}).


help() ->
    io:format("you can't help people who won't help themselves~n", []).


decompose(TxStr) ->
    case vd:decompose(TxStr) of
        {ok, X} ->
            io:format("~tp~n", [X]);
        {error, Error} ->
            io:format("ERROR: ~tp~n", [Error])
    end.


generate_keypair() ->
    #{public := PublicKey,
      secret := SecretKey} = ecu_eddsa:sign_keypair(),
    io:format("Public Key: ~w~n", [PublicKey]),
    io:format("Secret Key: ~w~n", [SecretKey]).
