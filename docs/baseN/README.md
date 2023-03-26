# Base58/Base64 Number Encoding Schema in Detail

Base64 and Base58 are two algorithms for encoding byte arrays in plain text. I
initially assumed these were two instances of the same "Base N" algorithm. **This
is not the case. These are two fundamentally different algorithms.**

## tldr

```erlang
-spec b64_enc(Bytes) -> Base64
    when Bytes  :: binary().
         Base64 :: string().
%% @doc
%% Encode a byte array into a base64 string
%% @end

%% general case: at least 3 bytes (24 bits) remaining
%% encode into 4 characters
b64_enc(<<A:6, B:6, C:6, D:6, Rest/binary>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B),
    CC = b64_int2char(C),
    CD = b64_int2char(D),
    [CA, CB, CC, CD | b64_enc(Rest)],
%% terminal case: 2 bytes (= 16 bits) remaining
%% encode into 3 characters and a single padding character
b64_enc(<<A:6, B:6, C:4>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B),
    CC = b64_int2char(C bsl 2),
    [CA, CB, CC, $=];
%% terminal case: 1 bytes (= 8 bits) remaining
%% encode into 2 characters and two padding characters
b64_enc(<<A:6, B:2>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B bsl 4),
    [CA, CB, $=, $=];
%% terminal case: empty byte array
b64_enc(<<>>) ->
    [].



-spec b64_dec(Base64) -> Bytes
    when Base64 :: string(),
         Bytes  :: binary().
%% @doc
%% Decode a base64 string into a byte array
%% @end

b64_dec(Base64_String) ->
    b64_dec(Base64_String, <<>>).


%% terminal case: two equals signs at the end (decode 1 byte)
b64_dec([W, X, $=, $=], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    <<LastByte:8, 0:4>> = <<NW:6, NX:6>>,
    <<Acc/binary, LastByte:8>>;
%% terminal case: one equals sign at the end (decode 2 bytes)
b64_dec([W, X, Y, $=], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    NY = b64_char2int(Y),
    <<B1:8, B2:8, 0:2>> = <<NW:6, NX:6, NY:6>>,
    <<Acc/binary, B1:8, B2:8>>;
%% terminal case: end of string
b64_dec([], Acc) ->
    Acc;
%% general case: 4 or more chars remaining (decode 3 bytes)
b64_dec([W, X, Y, Z | Rest], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    NY = b64_char2int(Y),
    NZ = b64_char2int(Z),
    NewAcc = <<Acc/binary, NW:6, NX:6, NY:6, NZ:6>>,
    b64_dec(Rest, NewAcc).



-spec b58_enc(Bytes) -> Base58
    when Bytes  :: binary(),
         Base58 :: string().
%% @doc
%% Encode a bytestring into base58 notation

b58_enc(Bytes) ->
    %% grab leading 0s
    {ZerosBase58, Rest}        = split_zeros(Bytes, []),
    NBitsInRest                = bit_size(Rest),
    <<RestBigNum:NBitsInRest>> = Rest,
    RestBase58                 = b58_enc(RestBigNum, []),
    ZerosBase58 ++ RestBase58.



-spec split_zeros(Bytes, B58_Zeros_Acc) -> {B58_Zeros, Rest}
    when Bytes         :: binary(),
         B58_Zeros_Acc :: string(),
         B58_Zeros     :: string(),
         Rest          :: binary().
%% @private
%% Base58 thinks of your byte array as a big integer, and therefore has no way
%% to distinguish between say <<1,2,3>> and <<0, 0, 0, 1, 2, 3>>. To resolve
%% this, we prepend ASCII `1`s at the beginning of the result, one for each
%% leading zero byte in the input byte array.
%%
%% The ASCII `0` (numeral zero) character is not used in order to avoid
%% ambiguity with ASCII `O` (uppercase letter O)

split_zeros(<<0:8, Rest/binary>>, B58_Zeros) ->
    split_zeros(Rest, [$1 | B58_Zeros]);
split_zeros(Rest, B58_Zeros) ->
    {B58_Zeros, Rest}.



-spec b58_enc(BytesBigNum, Base58Acc) -> Base58
    when BytesBigNum :: integer(),
         Base58Acc   :: [0..57],
         Base58      :: string().
%% @private
%% Encode a number into base58 notation using the standard quotient-remainder
%% algorithm you would use for any other base.

b58_enc(0, Acc) ->
    lists:map(fun b58_int2char/1, Acc);
b58_enc(BitNum, Acc) ->
    Q = BitNum div 58,
    R = BitNum rem 58,
    b58_enc(Q, [R | Acc]).



-spec b58_dec(Base58) -> DecodedBytes
    when Base58       :: string(),
         DecodedBytes :: binary().
%% @doc
%% Decode a Base58-encoded string into a bytestring
%% @end

%% this works by parsing the string as an integer and then converting the
%% integer to "base 256" (256 = 2^8)
b58_dec(Str) ->
    %% the number of leading 1 in the input plain-text string tells us the
    %% number of leading zeros in the output byte array
    {LeadingZeros, RestStr} = split_ones(Str, <<>>),
    %% you could make this more efficient by converting this to a single pass
    %% over the input string. steps shown separately because this is tutorial
    %% code
    RestNs                  = lists:map(fun b58_char2int/1, RestStr),
    RestBytes               = b58_dec(RestNs, 0),
    <<LeadingZeros/binary, RestBytes/binary>>.

%% this is basically the oppsite of split_zeros/2 above
split_ones([$1 | Rest], LeadingZeros) ->
    split_ones(Rest, <<LeadingZeros/binary, 1>>);
split_ones(B58Str, LeadingZeros) ->
    {LeadingZeros, B58Str}.


%% this parses the input symbols into a big integer
b58_dec([N | Ns], Acc) ->
    NewAcc = (Acc*58) + N,
    b58_dec(Ns, NewAcc);
%% then converts that big integer into "base 256" (i.e. a byte array)
b58_dec([], FinalAccN) ->
    bignum_to_binary_bige(FinalAccN, <<>>).

%% in the encode step, we were converting the number to a base58 "string"
%% here we are doing essentially the same thing, but instead converting the
%% number to a "base 256 string" (i.e. byte array)
bignum_to_binary_bige(0, Acc) ->
    Acc;
bignum_to_binary_bige(N, Acc) ->
    Q = N div 256,
    R = N rem 256,
    NewAcc = <<R, Acc/binary>>,
    bignum_to_binary_bige(Q, NewAcc).
```
