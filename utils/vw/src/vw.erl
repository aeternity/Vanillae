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
     "  mansplain APISTR    decompose the AE object and display it in a human-readable form\n"
     "      -e, --example: give an example\n"
     "      Example: vw mansplain tx\n"
    ].



-spec mansplain(ArgV) -> ok
    when ArgV :: [string()].
%% @private

mansplain(["-e"]) ->
    mansplain(["--example"]);
mansplain(["--example"]) ->
    mansplain([example_unsigned_tx_apistr()]);
mansplain(["tx_" ++ Base64Data]) ->
    {ok, TxBytes}   = vw_bnc:unbase64check(Base64Data),
    {RLPData, <<>>} = vrlp:decode(TxBytes),
    TaggedFields    = rlp2proplist(RLPData),
    display_tagged_fields(TaggedFields).

example_unsigned_tx_apistr() ->
    "tx_+LUrAaEBzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhMBoQWPFvtl5SACr++edEMrwJzoQp7/Tu6vJ3vxsdi9P5O+hQOGteYg9IAAAACCTiCEO5rKALhaKxH1lAXbW58AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITAJ8AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITOG+JA72RPmwd8//AoZ9UjA==".



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
    TagInt = decode_int(TagBytes),
    VsnInt = decode_int(VsnBytes),
    rlp2proplist(TagInt, VsnInt, Fields).


%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#table-of-object-tags
-define(OTAG_ContractCallTx, 43).


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
     {caller     , strview_id(CallerId_bytes)},
     {nonce      , strview_int(Nonce_bytes)},
     {contract   , strview_id(ContractId_bytes)},
     {abi_version, strview_int(ABIVersion_bytes)},
     {fee        , strview_int(Fee_bytes)},
     {ttl        , strview_int(TTL_bytes)},
     {amount     , strview_int(Amount_bytes)},
     {gas        , strview_int(Gas_bytes)},
     {gas_price  , strview_int(GasPrice_bytes)},
     {call_data  , strview_contract_bytes(CallData_bytes)}].



%%=============================================================================
%% strview functions: take data and get a one-line string representation
%%=============================================================================


-spec strview_id(AE_id) -> iolist()
    when AE_id :: binary().
%% @private
%% mansplain an id = show the ak_... nonsense
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type

strview_id(<<Tag, IdBytes:32/bytes>>) ->
    PrefixStr = prefix(Tag),
    IdStr     = vw_bnc:base58checked(IdBytes),
    PrefixStr ++ "_" ++ IdStr.


%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
prefix(1) -> "ak";
prefix(2) -> "nm";
prefix(3) -> "cm";
prefix(4) -> "ok";
prefix(5) -> "ct";
prefix(6) -> "ch".



-spec strview_int(IntBytes) -> iolist()
    when IntBytes :: binary().
%% @private
%% Take the bytes that encode an integer and format the integer

strview_int(IntBytes) ->
    integer_to_list(decode_int(IntBytes)).



-spec strview_contract_bytes(ContractBytes) -> iolist()
    when ContractBytes :: binary().
%% @private
%% Take a byte array and base64 encode it with the cb_... prefix

strview_contract_bytes(ContractBytes) ->
    ["cb_", vw_bnc:base64checked(ContractBytes)].



%% Decode functions

%% Take a bytestring and parse it as an int
decode_int(Bytes) ->
    binary:decode_unsigned(Bytes).
