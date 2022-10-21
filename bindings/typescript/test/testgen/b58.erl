%-module(b58).
%-export([enc/1, dec/1]).

-mode(compile).
%-spec enc(binary()) -> string().
%% https://digitalbazaar.github.io/base58-spec/#encode

main([]) ->
    {ok, Cases} = file:consult("b58_cases.eterms"),
    test_cases(Cases).

test_cases([{{encoded, E}, {decoded, D}} | Rest]) ->
    EncodeOk = E =:= enc(D),
    DecodeOk = D =:= dec(E),
    ok =
        case EncodeOk of
            true  -> ok;
            false -> io:format("===============================~n"
                               "YOU ARE A FAILURE TO ENCODE~n"
                               "===============================~n"
                               "decoded  : ~tw~n"
                               "expected : ~tw~n"
                               "actual   : ~tw~n~n",
                               [D, E, enc(D)])
        end,
    ok =
        case DecodeOk of
            true  -> ok;
            false -> io:format("===============================~n"
                               "YOU ARE A FAILURE TO DECODE~n"
                               "===============================~n"
                               "encoded  : ~tw~n"
                               "expected : ~tw~n"
                               "actual   : ~tw~n~n",
                               [E, D, dec(E)])
        end,
    test_cases(Rest);
test_cases([]) ->
    ok.

% this was much clearer: https://www.youtube.com/watch?v=GedV3S9X89c

enc(Bits) ->
    NBits = bit_size(Bits),
    <<BitNum:NBits>> = Bits,
    enc(BitNum, []).

enc(0, Acc) ->
    lists:map(fun int2char/1, Acc);
enc(BitNum, Acc) ->
    Q = BitNum div 58,
    R = BitNum rem 58,
    enc(Q, [R | Acc]).


dec(Str) ->
    Ns = lists:map(fun char2int/1, Str),
    dec(Ns, 0).

dec([N | Ns], Acc) ->
    NewAcc = (Acc*58) + N,
    dec(Ns, NewAcc);
dec([], FinalAccN) ->
    bignum_to_binary_bige(FinalAccN, <<>>).

bignum_to_binary_bige(0, Acc) ->
    Acc;
bignum_to_binary_bige(N, Acc) ->
    Q = N div 256,
    R = N rem 256,
    NewAcc = <<R, Acc/binary>>,
    bignum_to_binary_bige(Q, NewAcc).

int2char( 0) -> $1;
int2char( 1) -> $2;
int2char( 2) -> $3;
int2char( 3) -> $4;
int2char( 4) -> $5;
int2char( 5) -> $6;
int2char( 6) -> $7;
int2char( 7) -> $8;
int2char( 8) -> $9;
int2char( 9) -> $A;
int2char(10) -> $B;
int2char(11) -> $C;
int2char(12) -> $D;
int2char(13) -> $E;
int2char(14) -> $F;
int2char(15) -> $G;
int2char(16) -> $H;
int2char(17) -> $J;
int2char(18) -> $K;
int2char(19) -> $L;
int2char(20) -> $M;
int2char(21) -> $N;
int2char(22) -> $P;
int2char(23) -> $Q;
int2char(24) -> $R;
int2char(25) -> $S;
int2char(26) -> $T;
int2char(27) -> $U;
int2char(28) -> $V;
int2char(29) -> $W;
int2char(30) -> $X;
int2char(31) -> $Y;
int2char(32) -> $Z;
int2char(33) -> $a;
int2char(34) -> $b;
int2char(35) -> $c;
int2char(36) -> $d;
int2char(37) -> $e;
int2char(38) -> $f;
int2char(39) -> $g;
int2char(40) -> $h;
int2char(41) -> $i;
int2char(42) -> $j;
int2char(43) -> $k;
int2char(44) -> $m;
int2char(45) -> $n;
int2char(46) -> $o;
int2char(47) -> $p;
int2char(48) -> $q;
int2char(49) -> $r;
int2char(50) -> $s;
int2char(51) -> $t;
int2char(52) -> $u;
int2char(53) -> $v;
int2char(54) -> $w;
int2char(55) -> $x;
int2char(56) -> $y;
int2char(57) -> $z.

char2int($1) ->  0;
char2int($2) ->  1;
char2int($3) ->  2;
char2int($4) ->  3;
char2int($5) ->  4;
char2int($6) ->  5;
char2int($7) ->  6;
char2int($8) ->  7;
char2int($9) ->  8;
char2int($A) ->  9;
char2int($B) -> 10;
char2int($C) -> 11;
char2int($D) -> 12;
char2int($E) -> 13;
char2int($F) -> 14;
char2int($G) -> 15;
char2int($H) -> 16;
char2int($J) -> 17;
char2int($K) -> 18;
char2int($L) -> 19;
char2int($M) -> 20;
char2int($N) -> 21;
char2int($P) -> 22;
char2int($Q) -> 23;
char2int($R) -> 24;
char2int($S) -> 25;
char2int($T) -> 26;
char2int($U) -> 27;
char2int($V) -> 28;
char2int($W) -> 29;
char2int($X) -> 30;
char2int($Y) -> 31;
char2int($Z) -> 32;
char2int($a) -> 33;
char2int($b) -> 34;
char2int($c) -> 35;
char2int($d) -> 36;
char2int($e) -> 37;
char2int($f) -> 38;
char2int($g) -> 39;
char2int($h) -> 40;
char2int($i) -> 41;
char2int($j) -> 42;
char2int($k) -> 43;
char2int($m) -> 44;
char2int($n) -> 45;
char2int($o) -> 46;
char2int($p) -> 47;
char2int($q) -> 48;
char2int($r) -> 49;
char2int($s) -> 50;
char2int($t) -> 51;
char2int($u) -> 52;
char2int($v) -> 53;
char2int($w) -> 54;
char2int($x) -> 55;
char2int($y) -> 56;
char2int($z) -> 57.
