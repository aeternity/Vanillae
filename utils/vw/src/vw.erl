-module(vw).
-vsn("0.1.0").

-export([start/1]).
-compile([export_all, nowarn_export_all]).



-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    main(ArgV),
    zx:silent_stop().



-spec main(ArgV) -> ok
    when ArgV :: [string()].
%% @private
%% Dispatching function for vw

main(["mansplain" | Args]) -> mansplain(Args);
main(_)                    -> help().



-spec help() -> ok.
%% @private
%% Display the help screen

help() ->
    io:format("~ts", [help_screen()]).


help_screen() ->
    ["vw: vanillae wallet\n"
     "USAGE: vw COMMAND [ARGS]\n"
     "This is a simple command line Aeternity wallet (work in progress)\n"
     "\n"
     "COMMANDS\n"
     "  mansplain APISTR    decompose the AE object and display it in a human-readable awk/grep/etc-friendly form\n"
     "      -e, --examples      give some example call/return sequences\n"
    ].




-spec mansplain(ArgV) -> ok
    when ArgV :: [string()].
%% @private

mansplain(["-e"]) ->
    mansplain(["--examples"]);
mansplain(["--examples"]) ->
    ShowExample =
        fun(ExStr) ->
            io:format("$ vw mansplain ~ts~n", [ExStr]),
            mansplain([ExStr]),
            io:format("~n", [])
        end,
    lists:foreach(ShowExample, examples());
mansplain(["tx_" ++ Base64Data]) ->
    {ok, TxBytes}   = vw_bnc:unbase64check(Base64Data),
    {RLPData, <<>>} = vrlp:decode(TxBytes),
    TaggedFields    = rlp2proplist(RLPData),
    display_tagged_fields(TaggedFields).


examples() ->
    [ %% signed contract call
      "tx_+P8LAfhCuEBI0/elGvAObpwCvWBzCKU+xywg5ReHRaUEzgqilLANO3ZaivIIIEXn4meC1qdnuhND52z2/xfL56hnksCbr/0MuLf4tSsBoQHNl2HUwOipR4HsD929KgBGDov+csr3g+jxhhIHC0aSEwGhBY8W+2XlIAKv7550QyvAnOhCnv9O7q8ne/Gx2L0/k76FA4a15iD0gAAAAIJOIIQ7msoAuForEfWUBdtbnwCgzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhMAnwCgzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhM4b4kDvZE+bB3z/8C+FIYl"
      %% corresponding unsigned contract call TX
    , "tx_+LUrAaEBzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhMBoQWPFvtl5SACr++edEMrwJzoQp7/Tu6vJ3vxsdi9P5O+hQOGteYg9IAAAACCTiCEO5rKALhaKxH1lAXbW58AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITAJ8AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITOG+JA72RPmwd8//AoZ9UjA=="
    ].


-spec display_tagged_fields(TaggedFields) -> ok
    when TaggedFields :: [{atom(), iolist()}].
%% @private
%% Print the tagged fields in a awk/grep friendly way

display_tagged_fields(TaggedFields) ->
    io:format("~ts~n", [format_tagged_fields(TaggedFields)]).



-spec format_tagged_fields(TaggedFields) -> iolist()
    when TaggedFields :: [{atom(), iolist()}].
%% @private
%% Format the tagged fields in a awk/grep friendly way

format_tagged_fields(TaggedFields) ->
    %% need to find the max length of tags
    MaxTagLength = max_tag_length(TaggedFields),
    format_tagged_fields(MaxTagLength, TaggedFields).


format_tagged_fields(MaxTagLength, TaggedFields) ->
    format_tagged_fields(MaxTagLength, TaggedFields, []).


format_tagged_fields(MaxTagLength, [{ThisTag, ThisVal} | Rest], Acc) ->
    %% making it grep-friendly
    NewAcc = update_ftf_acc(Acc,
                            [format_tag(ThisTag, MaxTagLength), " ", ThisVal]),
    format_tagged_fields(MaxTagLength, Rest, NewAcc);
format_tagged_fields(_, [], Acc) ->
    Acc.


%% If there's already an accumulator, add a newline
%% otherwise don't
update_ftf_acc([], NewLn) ->
    NewLn;
update_ftf_acc(Acc, NewLn) ->
    [Acc, "\n", NewLn].


format_tag(ThisTagAtom, MaxTagLength) ->
    ThisTagStr     = atom_to_list(ThisTagAtom),
    ThisTagLength  = length(ThisTagStr),
    NumberOfSpaces = MaxTagLength - ThisTagLength,
    [ThisTagStr, spaces(NumberOfSpaces)].


spaces(NumberOfSpaces) ->
    spaces(NumberOfSpaces, []).


spaces(N, Acc) when N > 0 -> spaces(N-1, [Acc, " "]);
spaces(0, Acc)            -> Acc.


max_tag_length(TaggedFields) ->
    max_tag_length(TaggedFields, 0).


max_tag_length([{ThisTag, _Value} | Rest], CurrentMax) ->
    ThisTagLength = length(atom_to_list(ThisTag)),
    NewMax =
        case ThisTagLength > CurrentMax of
            true  -> ThisTagLength;
            false -> CurrentMax
        end,
    max_tag_length(Rest, NewMax);
max_tag_length([], Result) ->
    Result.



-type rlpdata()   :: binary() | [rlpdata()].
-type rlpfields() :: [rlpdata()].



-spec rlp2proplist(RawFields) -> Props
    when RawFields :: rlpfields(),
         Props     :: [{atom(), iolist()}].
%% @private
%% Take the list of fields and mansplain them
%%
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#binary-serialization

rlp2proplist([TagBytes, VsnBytes | Fields]) ->
    TagInt = binary:decode_unsigned(TagBytes),
    VsnInt = binary:decode_unsigned(VsnBytes),
    rlp2proplist(TagInt, VsnInt, Fields).


%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#table-of-object-tags
-define(OTAG_SignedTx, 11).
-define(OTAG_ContractCallTx, 43).


%% SignedTx version 1
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#signed-transaction
rlp2proplist(?OTAG_SignedTx, 1, Fields) ->
    [Signatures_list, %% [ <signatures>  :: [binary()]
     Tx_bytes]        %% , <transaction> :: binary()
        = Fields,
    [{object_type , "SignedTx"},
     {vsn         , "1"},
     {signatures  , vw_sv:signatures(Signatures_list)},
     {transaction , vw_sv:transaction(Tx_bytes)}];
%% ContractCallTx version 1
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#contract-call-transaction
rlp2proplist(?OTAG_ContractCallTx, 1, Fields) ->
    [CallerId_bytes,   %% [ <caller>      :: id()
     Nonce_bytes,      %% , <nonce>       :: int()
     ContractId_bytes, %% , <contract>    :: id()
     ABIVersion_bytes, %% , <abi_version> :: int()
     Fee_bytes,        %% , <fee>         :: int()
     TTL_bytes,        %% , <ttl>         :: int()
     Amount_bytes,     %% , <amount>      :: int()
     Gas_bytes,        %% , <gas>         :: int()
     GasPrice_bytes,   %% , <gas_price>   :: int()
     CallData_bytes]   %% , <call_data>   :: binary()
        = Fields,
    [{object_type, "ContractCallTx"},
     {vsn        , "1"},
     {caller     , vw_sv:id(CallerId_bytes)},
     {nonce      , vw_sv:int(Nonce_bytes)},
     {contract   , vw_sv:id(ContractId_bytes)},
     {abi_version, vw_sv:int(ABIVersion_bytes)},
     {fee        , vw_sv:int(Fee_bytes)},
     {ttl        , vw_sv:int(TTL_bytes)},
     {amount     , vw_sv:int(Amount_bytes)},
     {gas        , vw_sv:int(Gas_bytes)},
     {gas_price  , vw_sv:int(GasPrice_bytes)},
     {call_data  , vw_sv:contract_bytes(CallData_bytes)}].
