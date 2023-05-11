-module(bits).

-compile([export_all, nowarn_export_all]).


rand_bits(N) when N =< 8 ->
    <<Foo:N, _/bits>> = rand:bytes(1),
    <<Foo:N>>;
rand_bits(N) when N > 8 ->
    Num_Bytes         = N div 8,
    Num_Trailing_Bits = N rem 8,
    <<( rand_bits(Num_Trailing_Bits) )/bits,
      ( rand:bytes(Num_Bytes)        )/binary>>.



fmt(Bits) ->
    io:format("~ts~n", [format_bits(Bits)]).



format_bits(Bits) ->
    ["<<", format_bits2(Bits), ">>"].


format_bits2(Bits)                                          when 8 =< bit_size(Bits) ->
    NBits  = bit_size(Bits),
    NBytes = NBits div 8,
    NRem   = NBits rem 8,
    <<Bytes:NBytes/bytes, NewBits:NRem/bits>> = Bits,
    case NRem of
        0 ->
            format_bytes(Bytes);
        _ ->
            [format_bytes(Bytes), ", ", format_bits2(NewBits)]
    end;
format_bits2(Bits = <<Start:4/bits, Rem/bits>>)             when 5 =< bit_size(Bits) ->
    [format_bits2(Start), "_", format_bits2(Rem)];
format_bits2(Bits = <<B:1, Rest/bits>>)                     when 1 =< bit_size(Bits) ->
    case B of
        0 ->
            ["0", format_bits2(Rest)];
        1 ->
            ["1", format_bits2(Rest)]
    end;
format_bits2(<<>>) ->
    [].


%% hack: split into two groups of 4 bits
format_bytes(<<A:4/bits, B:4/bits>>) ->
    [format_bits2(A), "_", format_bits2(B)];
%% byte_size(Rest) >= 1 by previous case
format_bytes(<<N:1/bytes, Rest/bytes>>) ->
    [format_bytes(N), ", ", format_bytes(Rest)];
format_bytes(<<>>) ->
    [].



%% ten random cases for each pair of bit lengths, 0 =< N =< 8
rand_cases() ->
    rand_cases(0, 0, []).

%% ten random cases for 
rand_cases(8, 8, Acc) ->
    [ten_cases(8, 8) | Acc];
rand_cases(N, 8, Acc) ->
    NewAcc = [ten_cases(N, 8) | Acc],
    rand_cases(N + 1, 0, NewAcc);
rand_cases(N, M, Acc) ->
    rand_cases(N, M + 1, [ten_cases(N, M) | Acc]).


ten_cases(N, M) ->
    [cs(N, M) || _ <- lists:seq(1, 10)].

cs(N, M) ->
    A = rand_bits(N),
    B = rand_bits(M),
    C = <<A/bits, B/bits>>,
    {A, B, C}.
