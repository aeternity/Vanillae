-module(bits).

-compile([export_all, nowarn_export_all]).


empty() ->
    <<>>.

rand_bit() ->
    case rand:uniform(2) of
        1 -> <<0:1>>;
        2 -> <<1:1>>
    end.

rand_bits(N) when N =< 8 ->
    <<Foo:N, _/bits>> = rand:bytes(1),
    <<Foo:N>>.
rand_bits(N) when N > 8 ->
    Num_Bytes         = N div 8,
    Num_Trailing_Bits = N rem 8,
    <<( rand_bits(Num_Trailing_Bits) )/bits,
      ( rand:bytes(Num_Bytes)        )/binary>>.

