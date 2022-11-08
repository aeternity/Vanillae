%% @doc
%% Vanillae data humanization
%%
%% This is similar to serialization/deserialization, but not the same thing
%%
%% This code exists to work out concepts and code structure for Vanillae TS, it
%% may eventually become productized. Please do not use this.
%%
%% References:
%%
%% 1. https://github.com/aeternity/protocol/blob/master/serializations.md
%% 2. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
-module(vanth).

-compile([export_all, nowarn_export_all]).


%% semantic alias for "ak_" ++ string().
-type ak_str() :: string().
%% semantic alias for "tx_" ++ string().
-type tx_str() :: string().

%% TODO: expand
%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#the-id-type
-type anth_id() :: ak_str().

%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#spend-transaction
-type anth_spendtx() :: #{sender    := anth_id(),
                          recipient := anth_id(),
                          amount    := integer(),
                          fee       := integer(),
                          ttl       := integer(),
                          nonce     := integer(),
                          payload   := binary()}.

-spec humanize(API_String) -> HumanData
    when API_String :: tx_str(),
         HumanData  :: {ok, anth_spendtx()}
                     | {error, Reason :: term()}.
%% @doc
%% Humanize some data
%% @end

humanize("tx_" ++ Base64) ->
    hum_tx_b64(Base64);
humanize(X) ->
    {error, {nyi, X}}.


%% decode the base64 and check the hash thing
hum_tx_b64(B64_str) ->
    B64_Bytes    = list_to_binary(B64_str),
    %% This has the double sha at the end
    Stupid_Bytes = base64:decode(B64_Bytes),
    Stupid_Size  = byte_size(Stupid_Bytes),
    %% pull apart data
    <<RLP_encoded_data : (Stupid_Size - 4) /binary,
      Check            : 4                 /binary>> = Stupid_Bytes,
    ActualDoubleSha = shasha(RLP_encoded_data),
    case Check =:= ActualDoubleSha of
        false ->
            {error, checksum_mismatch};
        true ->
            decode_and_dispatch(RLP_encoded_data)
    end.

%% Double sha
shasha(Bytes) ->
    <<Result:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, Bytes)),
    Result.

%% decode rlp data
decode_and_dispatch(RLP_encoded_bytes) ->
    {DecodedData, Remainder} = vrlp:decode(RLP_encoded_bytes),
    case Remainder of
        <<>> -> hum_dispatch(DecodedData);
        _    -> {error, trailing_data}
    end.

%% at this point we have the rlp data, and based on the first field, we are
%% going to humanize the data

hum_dispatch([Tag_Bytes, Vsn_Bytes | Fields]) ->
    Tag = binary:decode_unsigned(Tag_Bytes),
    Vsn = binary:decode_unsigned(Vsn_Bytes),
    hd2(Tag, Vsn, Fields);
hum_dispatch(X) ->
    {error, {invalid_data, X}}.

%% 12 = spendtx, version = 1
hd2(_spendtx = 12, 1, Fields) ->
    hum_spendtx_fields(Fields);
hd2(Tag, Vsn, Fields) ->
    {error, {nyi, {hd2, Tag, Vsn, Fields}}}.

%% See: https://github.com/aeternity/protocol/blob/master/serializations.md#spend-transaction
hum_spendtx_fields([SenderBytes,
                    RecipBytes,
                    AmountBytes,
                    FeeBytes,
                    TTLBytes,
                    NonceBytes,
                    Payload]) ->
    SenderStr = humanize_id(SenderBytes),
    RecipStr  = humanize_id(RecipBytes),
    Amount    = binary:decode_unsigned(AmountBytes),
    Fee       = binary:decode_unsigned(FeeBytes),
    TTL       = binary:decode_unsigned(TTLBytes),
    Nonce     = binary:decode_unsigned(NonceBytes),
    {ok, #{sender    => SenderStr,
           recipient => RecipStr,
           amount    => Amount,
           fee       => Fee,
           ttl       => TTL,
           nonce     => Nonce,
           payload   => Payload}}.

humanize_id(<<1, IdBytes:32/binary>>) ->
    Check = shasha(IdBytes),
    Str = vb58:enc(<<IdBytes/binary, Check/binary>>),
    "ak_" ++ Str.
