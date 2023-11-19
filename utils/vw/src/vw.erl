%% @doc
%% This is a simple command line wallet
%%
%% The idea is to make one that's easy to script around
%%
%% Everything is awk/grep/etc friendly
%% @end
-module(vw).
-vsn("0.1.0").

-export([start/1]).
-compile([export_all, nowarn_export_all]).

%% TODO:
%%  1. mansplain -k (to get the value of a key)
%%  2. auto-chomp all arguments (so we can do "vw vtx $(vw mansplain -k signatures SIGNED_TX) ..."

%% stupid indentation here just so I can use vim's column number thing to keep
%% line widths sane in a way where I also don't have to do subtraction
help_screen() -> [
"vw: vanillae wallet\n"
"USAGE: vw COMMAND [ARGS]\n"
"This is a simple command line Aeternity wallet (work in progress)\n"
"\n"
"COMMANDS\n"
"  examples                 table of example data for testing/experimenting\n"
"  mansplain APISTR         decompose the AE object and display it in a\n"
"                           human-readable awk/grep/etc-friendly format\n"
"  vtx SIG NETID TX PUBKEY  verify that SIG is a valid signature for TX on\n"
"                           network NETID and the signature was by PUBKEY\n"
].


-spec start(ArgV) -> ok
    when ArgV :: [string()].

start(ArgV) ->
    dispatch(ArgV),
    zx:silent_stop().



-spec dispatch(ArgV) -> ok
    when ArgV :: [string()].
%% @private
%% Dispatching function for vw

dispatch(["examples" | Args])  -> examples(Args);
dispatch(["mansplain" | Args]) -> mansplain(Args);
dispatch(["vtx" | Args])       -> vtx(Args);
dispatch(_)                    -> io:format("~ts", [help_screen()]).



%%%---------------------------------------------------------------------------
%%% TOP-LEVEL COMMAND ENTRY POINTS
%%%---------------------------------------------------------------------------

-spec examples([]) -> ok.
%% @private
%% show a table of example datas

examples([]) ->
    Examples =
        [ %% unsigned contract call TX
          {'ContractCallTx', "tx_+LUrAaEBzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhMBoQWPFvtl5SACr++edEMrwJzoQp7/Tu6vJ3vxsdi9P5O+hQOGteYg9IAAAACCTiCEO5rKALhaKxH1lAXbW58AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITAJ8AoM2XYdTA6KlHgewP3b0qAEYOi/5yyveD6PGGEgcLRpITOG+JA72RPmwd8//AoZ9UjA=="}
          %% corresponding signed contract tx
        , {'SignedTx', "tx_+P8LAfhCuEBI0/elGvAObpwCvWBzCKU+xywg5ReHRaUEzgqilLANO3ZaivIIIEXn4meC1qdnuhND52z2/xfL56hnksCbr/0MuLf4tSsBoQHNl2HUwOipR4HsD929KgBGDov+csr3g+jxhhIHC0aSEwGhBY8W+2XlIAKv7550QyvAnOhCnv9O7q8ne/Gx2L0/k76FA4a15iD0gAAAAIJOIIQ7msoAuForEfWUBdtbnwCgzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhMAnwCgzZdh1MDoqUeB7A/dvSoARg6L/nLK94Po8YYSBwtGkhM4b4kDvZE+bB3z/8C+FIYl"}
        ],
    display_tagged_fields(Examples).



-spec mansplain(ArgV) -> ok
    when ArgV :: [string()].
%% @private
%% Take a piece of compound data and print its components in tabular form

mansplain(["tx_" ++ Base64Data]) ->
    {ok, TxBytes}   = vw_bnc:unbase64check(Base64Data),
    {RLPData, <<>>} = vrlp:decode(TxBytes),
    TaggedFields    = rlp2proplist(RLPData),
    display_tagged_fields(TaggedFields).



-spec vtx(ArgV) -> ok
    when ArgV :: [string()].
%% @private
%% verify that a signature is correct
%% @end

%% it's either the tx itself or its hash
%% Refs:
%% 1. Protocol: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/consensus/README.md#transaction-signature
%% 2. SDK Hashing: https://github.com/aeternity/aepp-sdk-js/blob/315718fbc34ab92df8b9fc04bf186d00f2af7151/src/utils/crypto.ts#L67-L74

vtx(["sg_" ++ SgStr, NetworkId, "tx_" ++ TxStr, "ak_" ++ AkStr]) ->
    {ok, SigBytes}    = vw_bnc:unbase58check(SgStr),
    {ok, TxBytes}     = vw_bnc:unbase64check(TxStr),
    {ok, PubkeyBytes} = vw_bnc:unbase58check(AkStr),
    Message1          = [NetworkId, TxBytes],
    %% try both
    Result =
        ecu_eddsa:sign_verify_detached(SigBytes, Message1, PubkeyBytes)
        orelse begin
            {ok, Hash}  = eblake2:blake2b(32, TxBytes),
            Message2    = [NetworkId, Hash],
            ecu_eddsa:sign_verify_detached(SigBytes, Message2, PubkeyBytes)
        end,
    io:format("~tp~n", [Result]).



%%%---------------------------------------------------------------------------
%%% MANSPLANATION PLUMBING
%%%---------------------------------------------------------------------------

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




%%%---------------------------------------------------------------------------
%%% TABLE PRINTER
%%%---------------------------------------------------------------------------

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
