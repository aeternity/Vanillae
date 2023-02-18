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


%parse(#{<<
