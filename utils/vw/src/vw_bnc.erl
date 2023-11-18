%% @doc
%% vw_bnc: baseNcheck encoding
%%
%% For encoding binary strings into base64/base58
%%
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md
-module(vw_bnc).

-export_type([
    bnc_error/0
]).


-export([
    shasha4/1,
    add_check_bytes/1,
    rm_check_bytes/1,
    base58checked/1,
    unbase58check/1,
    base64checked/1,
    unbase64check/1
]).


-type bnc_error() :: {{reason, checksum_bad},
                      {raw_bytes,           RawBytes          :: binary()},
                      {given_check_bytes,   AllegedCheckBytes :: binary()},
                      {correct_check_bytes, CorrectCheckBytes :: binary()}}.



-spec shasha4(Data) -> CheckBytes
    when Data       :: binary(),
         CheckBytes :: binary().
%% @doc
%% Take the sha256sum of data twice and return first 4 bytes

shasha4(Data) ->
    <<CheckBytes:4/bytes, _/bytes>> = crypto:hash(sha256, crypto:hash(sha256, Data)),
    CheckBytes.



-spec add_check_bytes(Bytes) -> CheckedBytes
    when Bytes        :: binary(),
         CheckedBytes :: binary().
%% @doc
%% Concat the shasha4 of the input and return it

add_check_bytes(Bytes) ->
    CheckBytes = shasha4(Bytes),
    <<Bytes/bytes, CheckBytes/bytes>>.



-spec rm_check_bytes(CheckedBytes) -> Result
    when CheckedBytes :: binary(),
         Result       :: {ok, RawBytes :: binary()}
                       | {error, bnc_error()}.
%% @doc
%% Delete the 4 shasha4 bytes off the tail end of the input
%% Check if they work

rm_check_bytes(CheckedBytes) ->
    RawBytesSize                    = byte_size(CheckedBytes) - 4,
    <<RawBytes:RawBytesSize/bytes,
      AllegedCheckBytes:4/bytes>>   = CheckedBytes,
    CorrectCheckBytes               = shasha4(RawBytes),
    case CorrectCheckBytes =:= AllegedCheckBytes of
        true  -> {ok, RawBytes};
        false -> {error, {{reason, checksum_bad},
                          {raw_bytes, RawBytes},
                          {given_check_bytes, AllegedCheckBytes},
                          {correct_check_bytes, CorrectCheckBytes}}}
    end.



-spec base58checked(Bytes) -> Base58Str
    when Bytes     :: binary(),
         Base58Str :: string().
%% @doc
%% Add the check bytes and base58-encode a bytestring

base58checked(Bytes) ->
    %% add check bytes
    vb58:enc(add_check_bytes(Bytes)).



-spec unbase58check(Base58Str) -> Result
    when Base58Str :: string(),
         Result       :: {ok, RawBytes :: binary()}
                       | {error, Reason :: bnc_error()}.
%% @doc
%% base58-decode a string and return the corresponding bytes
%% does the checking

unbase58check(Base58Str) ->
    rm_check_bytes(vb58:dec(Base58Str)).



-spec base64checked(Bytes) -> Base64Str
    when Bytes     :: binary(),
         Base64Str :: binary().
%% @doc
%% Add the check bytes and base64-decode a bytestring

base64checked(Bytes) ->
    base64:encode(add_check_bytes(Bytes)).



-spec unbase64check(Base64Data) -> Result
    when Base64Data :: string(),
         Result     :: {ok, Data :: binary()}
                     | {error, Reason :: bnc_error()}.
%% @private
%% Take base64-encoded data and make sure the check digits are good
%% Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md

unbase64check(Base64Str) ->
    rm_check_bytes(base64:decode(Base64Str)).
