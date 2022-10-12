# Base58/Base64 Number Encoding Schema in Detail

## tldr

```erlang
b58_enc(Bits) ->
    NBits = bit_size(Bits),
    <<BitNum:NBits>> = Bits,
    b58_enc(BitNum, []).

b58_enc(0, Acc) ->
    lists:map(fun b58_int2char/1, Acc);
b58_enc(BitNum, Acc) ->
    Q = BitNum div 58,
    R = BitNum rem 58,
    b58_enc(Q, [R | Acc]).



b58_dec(Str) ->
    Ns = lists:map(fun b58_char2int/1, Str),
    b58_dec(Ns, 0).

b58_dec([N | Ns], Acc) ->
    NewAcc = (Acc*58) + N,
    b58_dec(Ns, NewAcc);
b58_dec([], FinalAccN) ->
    MinNBits = trunc(math:log2(FinalAccN) + 1),
    NBytes   = ceil(MinNBits / 8),
    NBits    = NBytes * 8,
    <<FinalAccN:NBits>>.



b64_enc(<<A:6, B:6, C:6, D:6, Rest/binary>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B),
    CC = b64_int2char(C),
    CD = b64_int2char(D),
    [CA, CB, CC, CD | b64_enc(Rest)],
b64_enc(<<A:6, B:6, C:4>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B),
    CC = b64_int2char(C bsl 2),
    [CA, CB, CC, $=];
b64_enc(<<A:6, B:2>>) ->
    CA = b64_int2char(A),
    CB = b64_int2char(B bsl 4),
    [CA, CB, $=, $=];
b64_enc(<<>>) ->
    [].



b64_dec(Base64_String) ->
    b64_dec(Base64_String, <<>>).

b64_dec([W, X, $=, $=], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    <<LastByte:8, 0:4>> = <<NW:6, NX:6>>,
    <<Acc/binary, LastByte:8>>;
b64_dec([W, X, Y, $=], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    NY = b64_char2int(Y),
    <<B1:8, B2:8, 0:2>> = <<NW:6, NX:6, NY:6>>,
    <<Acc/binary, B1:8, B2:8>>;
b64_dec([], Acc) ->
    Acc;
b64_dec([W, X, Y, Z | Rest], Acc) ->
    NW = b64_char2int(W),
    NX = b64_char2int(X),
    NY = b64_char2int(Y),
    NZ = b64_char2int(Z),
    NewAcc = <<Acc/binary, NW:6, NX:6, NY:6, NZ:6>>,
    b64_dec(Rest, NewAcc).
```
