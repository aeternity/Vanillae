%%% @doc
%%% Vanillae Recursive Length Prefix: vrlp
%%%
%%% This module is currently named `vrlp', but you may want to change that.
%%% Remember that changing the name in `-module()' below requires renaming
%%% this file, and it is recommended to run `zx update .app` in the main
%%% project directory to make sure the ebin/vrlp.app file stays in
%%% sync with the project whenever you add, remove or rename a module.
%%% @end

-module(vrlp).
-vsn("0.1.0").
-author("Peter Harpending <peter.harpending@gmail.com>").
-copyright("Peter Harpending <peter.harpending@gmail.com>").
-license("ISC").

-export([hello/0]).


-spec hello() -> ok.

hello() ->
    io:format("~p (~p) says \"Hello!\"~n", [self(), ?MODULE]).
