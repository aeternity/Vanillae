-module(mpox).

-compile([export_all, nowarn_export_all]).


-spec pee_party() -> ok.
%% @doc
%% the main function

pee_party() ->
    ok = io:format("~tp~n", [file_map()]).



-spec file_map() -> map().
%% @doc
%% parse read the node api json file and return it as a map

file_map() ->
    {ok, B} = file:read_file("NodeApiV3_swagger.json"),
    {ok, D} = zj:binary_decode(B),
    D.


fmk() ->
    maps:keys(file_map()).


%% -record(ep,
%%         {url       :: binary(),
%%          method    :: binary(),
%%          params    :: binary(),
%%          responses :: map()}).

-spec test_path() -> map().

test_path() ->
    M = file_map(),
    P = maps:get(<<"paths">>, M),
    X = maps:filter(
            fun
                (<<"/accounts/{pubkey}/transactions/pending">>, _) ->
                    true;
                (_, _) ->
                    false
            end,
            P),
    X.

parse_paths(Map) ->
    lists:map(fun parse/1, maps:to_list(Map)).

parse({URL, Map}) ->
    Map2
        = #{method     := _,
            parameters := _,
            responses  := _}
        = parse2(Map),
    Map3 = maps:put(url, URL, Map2),
    Map3.


parse2(#{<<"get">> := Map}) ->
    Map2
        = #{parameters := _,
            responses  := _}
        = parse3(Map),
    Map3 = maps:put(method, get, Map2),
    Map3.

parse3(#{<<"parameters">> := P,
         <<"responses">>  := R}) ->
    #{parameters => P,
      responses  => R}.
