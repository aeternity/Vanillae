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

## Introduction

This document explains the Base58 and Base64 notations, and the algorithms
for working with them. I wrote this document because I had a fair bit of
difficulty working this out for myself, even with a strong math background.
I couldn't find any resource on the internet explaining all of this simply.

Code examples are given in Erlang and TypeScript.  These are the two most
common languages used within the Aeternity project, and both happen to be
languges that make these tasks easy.  This document assumes you are familiar
with either/both languages.  Even if that's not true, Erlang is a very simple
and clean language, so the code should be pretty self-explanatory if you read
it.

Base64 is kind of annoying but it's pretty straightforward to code.  My
initial assumption was that Base58 was in some way "the same" algorithm but
with `n = 58` instead of `n = 64`. When I went to look up the spec, I found
this ([source](https://digitalbazaar.github.io/base58-spec/))

> ### 3. The Base58 Encoding Algorithm
>
> To encode an array of bytes to a Base58 encoded value, run the following
> algorithm. All mathematical operations MUST be performed using integer
> arithmetic. Start by initializing a `zero_counter` to zero (`0x0`), an
> `encoding_flag` to zero (`0x0`), a `b58_bytes` array, a `b58_encoding`
> array, and a `carry` value to zero (`0x0`). For each byte in the array of
> bytes and while `carry` does not equal zero (`0x0`) after the first
> iteration:
>
> 1. If `encoding_flag` is not set, and if the byte is a zero (`0x0`),
>    increment the value of `zero_counter`. If the value is not zero `(0x0)`,
>    set `encoding_flag` to true `(0x1)`.
> 2. If `encoding_flag` is set, multiply the current byte value by 256 and add
>    it to `carry`.
> 3. Set the corresponding byte value in `b58_bytes` to the value of `carry`
>    modulus 58.
> 4. Set `carry` to the value of `carry` divided by 58.
>
> Once the `b58_bytes` array has been constructed, generate the final
> `b58_encoding` using the following algorithm. Set the first `zero_counter`
> bytes in `b58_encoding` to `1`. Then, for every byte in `b58_array`, map the
> byte value using the Base58 alphabet in the previous section to its
> corresponding character in `b58_encoding`. Return `b58_encoding` as the
> Base58 representation of the input array of bytes.

I personally have no idea what that does.  I found a YouTube video that
explained the Base58 algorithm in a way that made a lot more sense.
([source](https://youtu.be/GedV3S9X89c)). The video gave a clear enough
explanation of the Base58 algorithm that I could _figure out_ what is going on
and why it makes sense.  I was able to relate what I was seeing in the video to
background context I happen to have from mathematics.  But the video didn't
provide that context.

I want this document to explain what both algorithms do, why they make sense,
how they are different, and why they _have_ to be different.  All with code
examples and sufficient mathematical context.

Let's get started.

Any data stored in a computer is represented as an integer. For the purposes of
this discussion, we're going to assume all integers are non-negative (greater
than or equal to 0).  The discussion below can easily be modified to accomodate
negative integers.  This would add a small amount of annoying complexity in
exchange for no gain in conceptual clarity.  Nothing we are doing requires
dealing with negative numbers.

The problem we are interested in is _how do we represent really big integers in
plain text?_.

The first point I want you to take away is that **these are two totally
different solutions**.  Do not be fooled by the name.  It is **NOT** the
case that these are two instances of the same "Base N"
algorithm, just one is `N = 64` and one is `N = 58`. **These are two totally
different approaches to solving the same problem.**

More precisely, the underlying mathematics behind the two notations is very
similar, but the algorithms for producing them are very different. More detail
later.

Like I said, any given piece of data is---from the perspective of your
computer---just a very big integer. The difference between the two algorithms
is, roughly:

1. The Base64 algorithm thinks of that integer as a "stream of digits"
2. The Base58 algorithm thinks of that integer as a "pure integer," kind of
   the way math thinks of an integer: the integer _itself_ is a different
   thing than the way the integer is _represented_.

Base64 encoding/decoding involves a straightforward translation back and forth
from the machine representation of integers, without thinking too much (or at
all) about the math involved.

Base58 encoding/decoding requires thinking about the integer from a more mathy
point of view.  That weird arcane algorithm above is what happens when you try
to phrase the mathematics in terms of the machine representation of
really big integers.

We're going to focus on the mathy point of view and then circle back to the
weird arcane algorithm later on.

There is actually a good reason we don't use decimal notation for really big
integers: it's extremely wasteful.

I'll explain the following in more detail in a later section. Roll with me. To
any piece of data there is associated a quantity called **information**.  The
_unit_ of information is the _bit_, in the same sense that the unit of length
is the meter.

1. There are 256 distinct bytes. A single byte (machine digit)
   contains exactly 8 bits $8 = \log_2 256$ of information.

2. There are 10 distinct decimal ("Base10") symbols. A single decimal digit
   contains approximately 3.32 bits (`3.32 ~ log2(10)`) of information.

3. There are 64 distinct Base64 symbols. A single Base64 digit contains
   exactly $6$ bits (`6 = log2(64)`) of information.

4. There are 58 distinct Base58 symbols. A single Base58 digit contains
   approximately 5.86 bits (`5.86 ~ log2(58)`) of information.

What this means is, in base64 notation, each symbol consumes 6 bits of
information, roughly twice the rate of decimal notation (~3.32 bits per
symbol).  What this means in practice is that a number written in Base64
notation is about half as long as a number written in decimal notation.

For instance, the number `K = 90 682 877 680 429`

1. requires 14 digits (count them!) in decimal notation

   $$
        \frac{(\log_2  K) \text{ bits}}
             {(\log_2 10) \text{ bits per symbol}}
        \approx
            \frac{46.37 \text{ bits}}
                 { 3.37 \text{ bits per symbol}}
        \approx 13.96 \text{ symbols}
   $$

2. requires 8 digits in Base64 notation (`UnnAthst`)

   $$
       \frac{(\log_2  K) \text{ bits}}
            {(\log_2 64) \text{ bits per symbol}}
       \approx
           \frac{46.37 \text{ bits}}
                { 6    \text{ bits per symbol}}
       \approx 7.73 \text{ symbols}
   $$

3. requires 8 digits in Base58 notation (`i55xNZNt`)

   $$
       \frac{(\log_2  K) \text{ bits}}
            {(\log_2 58) \text{ bits per symbol}}
       \approx
           \frac{46.37 \text{ bits}}
                { 5.86 \text{ bits per symbol}}
       \approx 7.91 \text{ symbols}
   $$

As you can see, the difference in space complexity between Base58 and Base64 is
pretty small, but the difference between Base10 is pretty large.  Base58 has
the same alphabet (set of symbols) as Base64, minus a handful that can cause
readability or manual input issues.  For instance, the Base64 alphabet contains
both the symbol `0` (numeral zero) and `O` (uppercase letter `o`).  The Base58
alphabet contains neither.
