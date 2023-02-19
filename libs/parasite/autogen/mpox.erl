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



-record(ep_get,
        {function_name  :: binary(),
         description    :: binary(),
         parameters     :: [ParamTypeName :: binary()],
         response_types :: [{HttpCode         :: integer(),
                             ResponseType     :: {ref, TypeName :: binary()}
                                               | {inline, Type :: map()},
                             ResponseDesc     :: binary()}],
         url            :: binary()}).


-record(ep_post,
        {function_name     :: binary(),
         description       :: binary(),
         parameters        :: [ParamTypeName :: binary()],
         request_body_type :: {ref, TypeName :: binary()},
         response_types    :: [{HttpCode         :: integer(),
                                ResponseType     :: {ref, TypeName :: binary()}
                                                  | {inline, Type :: map()}
                                                  | none,
                                ResponseDesc     :: binary()}],
         url               :: binary()}).


-record(ep_delete,
        {function_name     :: binary(),
         description       :: binary(),
         parameters        :: [ParamTypeName :: binary()],
%         request_body_type :: {ref, TypeName :: binary()},
         response_types    :: [{HttpCode         :: integer(),
                                ResponseType     :: {ref, TypeName :: binary()}
                                                  | {inline, Type :: map()}
                                                  | none,
                                ResponseDesc     :: binary()}],
         url               :: binary()}).


-type endpoint() :: #ep_get{}
                  | #ep_post{}
                  | #ep_delete{}.



-spec parse_paths(Paths) -> Result
    when Paths  :: map(),
         Result :: [endpoint()].


parse_paths(Map) ->
    lists:map(fun parse/1, maps:to_list(Map)).



-spec parse(EndpointSpec) -> Endpoint
    when EndpointSpec :: {URL :: binary(), EPMap :: map()},
         Endpoint     :: endpoint().


parse({URL, #{<<"get">> := Map}}) ->
    #{function_name   := FunName,
       description    := Desc,
       parameters     := Params,
       response_types := RTs} = parse2_get(Map),
    #ep_get{function_name  = FunName,
            description    = Desc,
            parameters     = Params,
            response_types = RTs,
            url            = URL};
parse({URL, #{<<"post">> := Map}}) ->
    #{function_name      := FunName,
       description       := Desc,
       parameters        := Params,
       request_body_type := ReqBodyType,
       response_types    := RTs} = parse2_post(Map),
    #ep_post{function_name     = FunName,
             description       = Desc,
             parameters        = Params,
             request_body_type = ReqBodyType,
             response_types    = RTs,
             url               = URL}.

%% AHh.... hmm need to think about the keys... this case-splitting thing isn't 
%parse({URL, #{<<"delete">> := Map}}) ->
%    #{function_name      := FunName,
%       description       := Desc,
%       parameters        := Params,
%       response_types    := RTs} = parse2_delete(Map),
%    #ep_delete{function_name     = FunName,
%               description       = Desc,
%               parameters        = Params,
%               request_body_type = ReqBodyType,
%               response_types    = RTs,
%               url               = URL}.
%

parse2_get(#{<<"operationId">> := FunctionName,
             <<"description">> := Desc,
             <<"parameters">>  := Params,
             <<"responses">>   := Resps}) ->
    #{function_name   => FunctionName,
      description     => Desc,
      parameters      => parse_parameters(Params),
      response_types  => parse_response_types(Resps)};
% no parameters
parse2_get(#{<<"operationId">> := FunctionName,
             <<"description">> := Desc,
             <<"responses">>   := Resps}) ->
    #{function_name   => FunctionName,
      description     => Desc,
      parameters      => [],
      response_types  => parse_response_types(Resps)};
% no description
parse2_get(#{<<"operationId">> := FunctionName,
             <<"parameters">>  := Params,
             <<"responses">>   := Resps}) ->
    #{function_name   => FunctionName,
      description     => <<"(no description given)">>,
      parameters      => parse_parameters(Params),
      response_types  => parse_response_types(Resps)}.


parse2_post(#{<<"operationId">> := FunctionName,
              <<"description">> := Desc,
              <<"parameters">>  := Params,
              <<"requestBody">> := RB,
              <<"responses">>   := Resps}) ->
    #{function_name     => FunctionName,
      description       => Desc,
      parameters        => parse_parameters(Params),
      request_body_type => parse_request_body_type(RB),
      response_types    => parse_response_types(Resps)};
%% no parameters field
parse2_post(#{<<"operationId">> := FunctionName,
              <<"description">> := Desc,
              <<"requestBody">> := RB,
              <<"responses">>   := Resps}) ->
    #{function_name     => FunctionName,
      description       => Desc,
      parameters        => [],
      request_body_type => parse_request_body_type(RB),
      response_types    => parse_response_types(Resps)}.


parse_request_body_type(#{<<"content">> := #{<<"application/json">> := #{<<"schema">> := #{<<"$ref">> := <<"#/components/schemas/", Name/binary>>}}}}) ->
    {ref, Name}.

parse_parameters(Params) ->
    lists:map(fun parse_parameter/1, Params).



-spec parse_parameter(ParamMap) -> Param
    when ParamMap :: map(),
         Param    :: {ref, Name :: binary()}
                   | {inline, Type :: map()}.

parse_parameter(#{<<"$ref">> := <<"#/components/parameters/", ParamTypeName/binary>>}) ->
    {ref, ParamTypeName};
parse_parameter(Map) when is_map(Map) ->
    {inline, Map}.



parse_response_types(Types) ->
    lists:map(fun parse_response_type/1, maps:to_list(Types)).

parse_response_type({HttpCode_bin, #{<<"content">>    := #{<<"application/json">> := #{<<"schema">> := #{<<"$ref">> := <<"#/components/schemas/", ResponseTypeName/binary>>}}},
                                     <<"description">> := ResponseDesc}}) ->
    {erlang:binary_to_integer(HttpCode_bin),
     {ref, ResponseTypeName},
     ResponseDesc};
% no response success type
parse_response_type({<<"200">>, #{<<"description">> := ResponseDesc}}) ->
    {200, none, ResponseDesc}.


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
