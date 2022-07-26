%%% @doc
%%% Vanillae for Erlang Client Service Supervisor
%%%
%%% This is the service-level supervisor of the system. It is the parent of both the
%%% client connection handlers and the client manager (which manages the client
%%% connection handlers). This is the child of v_sup.
%%%
%%% See: http://erlang.org/doc/apps/kernel/application.html
%%% @end

-module(v_clients).
-vsn("0.1.0").
-behavior(supervisor).
-author("Craig Everett <ceverett@tsuriai.jp>").
-copyright("Craig Everett <ceverett@tsuriai.jp>").
-license("MIT").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, none).

-spec init(none) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init(none) ->
    RestartStrategy = {rest_for_one, 1, 60},
    ClientMan = {v_client_man,
                 {v_client_man, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [v_client_man]},
    ClientSup = {v_client_sup,
                 {v_client_sup, start_link, []},
                 permanent,
                 5000,
                 supervisor,
                 [v_client_sup]},
    Children  = [ClientSup, ClientMan],
    {ok, {RestartStrategy, Children}}.
