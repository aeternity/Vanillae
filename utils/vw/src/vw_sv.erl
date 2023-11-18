%% @doc
%% Field string views of AE binary/RLP data
%%
%% The input to every function is binary (or a list of binary items)
%%
%% The output of every function is an iolist that has no whitespace
%%
%% The idea is that these can be fields which are awk/grep/etc-friendly
%%
%% Lists are comma-separated
-module(vw_sv).

-export([
    contract_bytes/1,
    id/1,
    int/1,
    signature/1,
    signatures/1,
    transaction/1
]).



-spec contract_bytes(ContractBytes) -> iolist()
    when ContractBytes :: binary().
%% @private
%% Take a byte array and base64 encode it with the cb_... prefix

contract_bytes(ContractBytes) ->
    ["cb_", vw_bnc:base64checked(ContractBytes)].



-spec id(AE_id) -> iolist()
    when AE_id :: binary().
%% @private
%% string view of an id = ak_... nonsense (or nm_ etc depending on the tag
%% prefix)
%%
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type

id(<<Tag, IdBytes:32/bytes>>) ->
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



-spec int(IntBytes) -> iolist()
    when IntBytes :: binary().
%% @private
%% Take the bytes that encode an integer and format the integer

int(IntBytes) ->
    integer_to_list(binary:decode_unsigned(IntBytes)).



-spec signature(Sig) -> iolist()
    when Sig :: binary().
%% @doc
%% encode in sg_... notation
%% (base 58)
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md

signature(SigBytes) ->
    ["sg_", vw_bnc:base58checked(SigBytes)].



-spec signatures(Sigs) -> iolist()
    when Sigs :: [binary()].
%% @doc
%% Take a list of signatures
%% For each signature, encode in sg_... notation
%% Separate by ,
%% @end

%% this case should not occur in practice
%% but if someone calls this with an empty list, oblige
signatures(Sigs) ->
    comma_list(fun signature/1, Sigs).



-spec transaction(TxBytes) -> iolist()
    when TxBytes :: binary().
%% @doc
%% Encode a transaction in tx_... notation

transaction(TxBytes) ->
    %% tx is base64 encoded
    %% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md
    ["tx_", vw_bnc:base64checked(TxBytes)].



%%-----------------------------------------------------------------------------
%% INTERNALS
%%-----------------------------------------------------------------------------

-spec comma_list(EncodeFn, Items) -> iolist()
    when Items    :: [Item],
         Item     :: binary(),
         EncodeFn :: fun((binary()) -> iolist()).
%% @private
%% Given a function that produces a string-view of a binary, and a list of such
%% binaries, produce a comma-separated list encoding each item (no spaces)

%% encode a comma-separated list
%% Generally we will get nonempty lists, but empty lists are encoded as empty
%% strings
comma_list(_, []) ->
    "";
%% Exactly one item left, just encode/return it
comma_list(EncodeFn, [Item]) ->
    EncodeFn(Item);
%% At least two items left, intersperse a comma
comma_list(EncodeFn, [Item | Rest]) ->
    [EncodeFn(Item), ",", comma_list(EncodeFn, Rest)].

