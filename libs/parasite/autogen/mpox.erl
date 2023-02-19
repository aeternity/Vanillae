-module(mpox).

-compile([export_all, nowarn_export_all]).


-spec main() -> ok.
%% @doc
%% the main function

main() ->
    parse_paths(test_paths()).



-spec file_map() -> map().
%% @doc
%% parse read the node api json file and return it as a map

file_map() ->
    {ok, B} = file:read_file("NodeApiV3_swagger.json"),
    {ok, D} = zj:binary_decode(B),
    D.


fmk() ->
    maps:keys(file_map()).



-spec paths() -> map().

paths() ->
    M = file_map(),
    maps:get(<<"paths">>, M).



-spec test_paths() -> map().

test_paths() ->
    IsOurPath =
        fun
            (<<"/accounts/{pubkey}/transactions/pending">>, _) ->
                true;
            (_, _) ->
                false
        end,
    maps:filter(IsOurPath, paths()).



-record(ep,
        {function_name  :: binary(),
         description    :: binary(),
         parameters     :: [ParamTypeName :: binary()],
         response_types :: [{HttpCode         :: integer(),
                             ResponseTypeName :: binary(),
                             ResponseDesc     :: binary()}],
         url            :: binary(),
         method         :: binary()}).


-type endpoint() :: #ep{}.


-spec parse_paths(Paths) -> Result
    when Paths  :: map(),
         Result :: [endpoint()].


parse_paths(Map) ->
    lists:map(fun parse/1, maps:to_list(Map)).



-spec parse(EndpointSpec) -> Endpoint
    when EndpointSpec :: {URL :: binary(), EPMap :: map()},
         Endpoint     :: endpoint().


parse({URL, Map}) ->
    #{function_name  := FunName,
       description    := Desc,
       parameters     := Params,
       response_types := RTs,
       % url is from input
       method         := Method} = parse2(Map),
    #ep{function_name  = FunName,
        description    = Desc,
        parameters     = Params,
        response_types = RTs,
        url            = URL,
        method         = Method}.


parse2(#{<<"get">> := Map}) ->
    Map2
        = #{function_name  := _,
            description    := _,
            parameters     := _,
            response_types := _}
        = parse3(Map),
    Map3 = maps:put(method, get, Map2),
    Map3.



parse3(#{<<"operationId">> := FunctionName,
         <<"description">> := Desc,
         <<"parameters">>  := Params,
         <<"responses">>   := Resps}) ->
    #{function_name   => FunctionName,
      description     => Desc,
      parameters      => parse_parameters(Params),
      response_types  => parse_response_types(Resps)}.



parse_parameters(Params) ->
    lists:map(fun parse_parameter/1, Params).

parse_parameter(#{<<"$ref">> := <<"#/components/parameters/", ParamTypeName/binary>>}) ->
    ParamTypeName.



parse_response_types(Types) ->
    lists:map(fun parse_response_type/1, maps:to_list(Types)).

parse_response_type({HttpCode_bin, #{<<"content">>    := #{<<"application/json">> := #{<<"schema">> := #{<<"$ref">> := <<"#/components/schemas/", ResponseTypeName/binary>>}}},
                                     <<"description">> := ResponseDesc}}) ->
    {erlang:binary_to_integer(HttpCode_bin),
     ResponseTypeName,
     ResponseDesc}.


%% current status:
%%
%% 70> mpox:main().
%% [{ep,<<"GetPendingAccountTransactionsByPubkey">>,
%%      <<"Get pending account transactions by public key">>,
%%      [<<"intAsString">>,<<"accountPubkey">>],
%%      [{200,<<"SignedTxs">>,<<"Successful operation">>},
%%       {400,<<"Error">>,<<"Invalid public key">>},
%%       {404,<<"Error">>,<<"Account not found">>}],
%%      <<"/accounts/{pubkey}/transactions/pending">>,get}]
%% 71> mpox:parse_paths(mpox:paths()).
%% ** exception error: no function clause matching
%%                     mpox:parse_parameter(#{<<"description">> =>
%%                                                <<"What strategy to use in order to determine the next nonce: shall it check for continuity or return the large"...>>,
%%                                            <<"in">> => <<"query">>,<<"name">> => <<"strategy">>,
%%                                            <<"required">> => false,
%%                                            <<"schema">> =>
%%                                                #{<<"default">> => <<"max">>,
%%                                                  <<"enum">> => [<<"max">>,<<"continuity">>],
%%                                                  <<"example">> => <<"max">>,<<"type">> => <<"string">>}}) (mpox.erl, line 122)
%%      in function  lists:map/2 (lists.erl, line 1243)
%%      in call from lists:map/2 (lists.erl, line 1243)
%%      in call from mpox:parse3/1 (mpox.erl, line 114)
%%      in call from mpox:parse2/1 (mpox.erl, line 102)
%%      in call from mpox:parse/1 (mpox.erl, line 87)
%%      in call from lists:map/2 (lists.erl, line 1243)
%%      in call from lists:map/2 (lists.erl, line 1243)
%%
%% not done, but good progress.
