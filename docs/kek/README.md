# Keccak, SHA-3, and SHAKE-N algorithms explained, with code in Erlang

![I can't make a diagram for this one, fellas.[^sponge]](./sponge.jpg)

Keccak is a hashing algorithm used for the SHA-3 standard. [The NIST
standard][nist-standard] is semi-readable math clownery. Hashing algorithms by
nature have to be complicated and somewhat obfuscated, so this is kind of par
for the course.

I found [this lecture][german-lecture] and the [accompanying
notes][german-lecture-notes] indispensable when writing kek.

At the end of the day, all Keccak does is take in some input bits, process them
in a deterministic way, and hand you output bits.

The goal of this document is just to explain what the processing steps are as
straightforwardly as possible.

Keccak is the "general case", and then SHA-3 and SHAKE-128 and so on are
special cases of Keccak.

## tldr

- [Erlang code (clear)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl)
- [Erlang code (fast)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek_fast.erl)
- [erlang-sha3 library (uses fast version of kek)](https://github.com/zxq9/erlang-sha3/blob/63193654e3c05d8031300ffcd52092f75e8b5c2f/src/sha3.erl#L85-L112)

#### References

1. [Helpful lecture][german-lecture]
2. [Notes for that lecture][german-lecture-notes]
3. [NIST standard][nist-standard] (btw: the double bar notation means "concatenate")
4. [SHA-3 Wikipedia](https://en.wikipedia.org/wiki/SHA-3)

#### Pitfall: "fast keccak" versus "clear keccak"

The main reference is the "clear" Erlang code.  The "fast" version is the
"clear" version with optimizations applied.  The fast version was written by
Hans Svensson.

In particular, fast keccak is arity 4, and clear keccak is arity 3.

There are a couple of trivial differences in code structure, so when you're
reading code, you need to keep in mind which version you're looking at. When I
have a code sample, I will link the GitHub permalink in a comment where the
code originates from, so there shouldn't be any confusion.

```erlang
%% From "clear keccak"
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L155-L159
-spec keccak(Capacity, Message, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().

%% From "fast Keccak"
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek_fast.erl#L142-L147
-spec keccak(Capacity, Message, Delimiter, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         Delimiter       :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
```

The reason for this is one of Hans's optimizations was speeding up the manner
in which padding was applied, and that required factoring it out into an
argument.

#### Pitfall: "NIST SHA-3" versus "Keccak SHA-3"

There are two versions of "SHA-3".  There's the "NIST version" and the "keccak
version".  The only difference is that NIST adds two bits (yes bits, not bytes)
of padding

```erlang
%% From the erlang-sha3 lib, which uses the fast version
%% - ignore the capacity and what not for now, will be explained in a minute
%% - OutputBitLength is the N in SHA3-N. So it would be 512 for SHA3-512, 256
%%   for SHA3-256, etc.
%% - Message is the input bitstring to be hashed

%% https://github.com/zxq9/erlang-sha3/blob/63193654e3c05d8031300ffcd52092f75e8b5c2f/src/sha3.erl#L107-L112
kek(OutputBitLength, Message, keccak) ->
    Capacity = 2 * OutputBitLength,
    keccak(Capacity, Message, <<>>, OutputBitLength);
                          %%  ^ keccak sha3 adds no padding bits
kek(OutputBitLength, Message, nist) ->
    Capacity = 2 * OutputBitLength,
    keccak(Capacity, Message, <<2#01:2>>, OutputBitLength).
                          %%  ^ nist sha3 concats these padding bits to the end of Message
```

The reason this problem exists is the "keccak version" was used in production
code before the NIST standard was published. So now there are two versions
of "SHA-3" out there.

#### Pitfall: SHAKE-N versus SHA3-N

Note that the padding and sha3-is-keccak-but-its-not pitfall only applies to
the SHA-3 fixed-length hashing functions. These produce a fixed-length (say,
512 bit) hash.

The SHAKE-N algorithms are sort of "arbitrary length SHA-3". So instead of
producing 512 bits, you tell it how long the output length should be.  The
SHAKE-N algorithms have their own padding rule, and I don't believe they're
affected by the padding thing.

```erlang
%% From the "clear" version
%% The clear version was written against the NIST standard

%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L86-L89
sha3(OutputBitLength, Message) ->
    Capacity = 2*OutputBitLength,
    %% SHA-3 NIST padding is used in the clear version
    ShaMessage = <<Message/bitstring, (2#01):2>>,
    keccak(Capacity, ShaMessage, OutputBitLength).

%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L131-L134
shake(ShakeNumber, Message, OutputBitLength) ->
    Capacity = 2*ShakeNumber,
    %% SHAKE has its own padding rule
    ShakeMessage = <<Message/bitstring, (2#1111):4>>,
    keccak(Capacity, ShakeMessage, OutputBitLength).
```

## SHA-s and SHAKE-s

These are the "porcelain" functions that we show to the outside world.  These
are just rewrites in front of Keccak.  Another way to think about it is that
Keccak has tons of settings, and each of these "algorithms" are just different
settings presets.

```erlang
%% From: https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L26-L134

-spec sha3_224(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:224>>.
%% @doc
%% SHA-3 with an output bit length of 224 bits.
%% @end

sha3_224(Message) ->
    sha3(224, Message).



-spec sha3_256(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:256>>.
%% @doc
%% SHA-3 with an output bit length of 256 bits.
%% @end

sha3_256(Message) ->
    sha3(256, Message).



-spec sha3_384(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:384>>.
%% @doc
%% SHA-3 with an output bit length of 384 bits.
%% @end

sha3_384(Message) ->
    sha3(384, Message).



-spec sha3_512(Message) -> Digest
    when Message :: bitstring(),
         Digest  :: <<_:512>>.
%% @doc
%% SHA-3 with an output bit length of 512 bits.
%% @end

sha3_512(Message) ->
    sha3(512, Message).



-spec sha3(OutputBitLength, Message) -> Digest
    when OutputBitLength :: pos_integer(),
         Message         :: bitstring(),
         Digest          :: bitstring().
%% @doc
%% SHA-3 with an arbitrary output bit length.
%%
%% This means Keccak with Capacity = 2*OutputBitLength. Additionally, SHA3
%% concatenates the bits 01 onto the end of the input, before sending the
%% Message to keccak/3.
%% @end

sha3(OutputBitLength, Message) ->
    Capacity = 2*OutputBitLength,
    ShaMessage = <<Message/bitstring, (2#01):2>>,
    keccak(Capacity, ShaMessage, OutputBitLength).



-spec shake128(Message, OutputBitLength) -> Digest
    when Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 256 = 2*128 bits.
%% @end

shake128(Message, OutputBitLength) ->
    shake(128, Message, OutputBitLength).



-spec shake256(Message, OutputBitLength) -> Digest
    when Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 512 = 2*256 bits.
%% @end

shake256(Message, OutputBitLength) ->
    shake(256, Message, OutputBitLength).



-spec shake(ShakeNumber, Message, OutputBitLength) -> Digest
    when ShakeNumber     :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% This is the SHAKE variable-length hash with Capacity 512 = 2*ShakeNumber bits.
%%
%% This concatenates the bitstring 1111 onto the end of the Message before
%% sending the message to keccak/3.
%% @end

shake(ShakeNumber, Message, OutputBitLength) ->
    Capacity = 2*ShakeNumber,
    ShakeMessage = <<Message/bitstring, (2#1111):4>>,
    keccak(Capacity, ShakeMessage, OutputBitLength).
```

## Outer Keccak

Keccak uses this "sponge" metaphor.  If you think of the bits as water, the
state type of Keccak is somewhat analagous to a sponge. In this analogy, the
Keccak algorithm consists of "absorbing" the bits into the sponge, and then
"squeezing" the bits out.

Both the absorption and squeezing phases invoke "inner keccak", which is where
all the real bit-churning happens. (These are the Greek letter steps in the
standard).

Just pay attention to the flow for now.

- We take in our input (`Message`).
- Add some padding bits to get `PaddedMessage`.
- `InitialSponge` is a 1600-bit long array of `0` bits (the "dry sponge").
- We make the dry `InitialSponge` wet by `absorb/4`ing the `PaddedMessage`, to create `WetSponge`
- We `squeeze/3` the `WetSponge` out to get the `ResultBits`

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L155-L172

-spec keccak(Capacity, Message, OutputBitLength) -> Digest
    when Capacity        :: pos_integer(),
         Message         :: bitstring(),
         OutputBitLength :: pos_integer(),
         Digest          :: bitstring().
%% @doc
%% Note: this is Keccak 1600, the only one used in practice
%%
%% Capacity must be strictly less than 1600
%% @end

keccak(Capacity = _c, Message, OutputBitLength) ->
    BitRate       = 1600 - Capacity,
    PaddedMessage = pad(Message, BitRate),
    InitialSponge = <<0:1600>>,
    WetSponge     = absorb(PaddedMessage, BitRate, Capacity, InitialSponge),
    ResultBits    = squeeze(WetSponge, OutputBitLength, BitRate),
    ResultBits.
```

The padding part is kind of dumb. `absorb/4` and `squeeze/3` both call
`inner_keccak/1`, which like I said is where all the real bit-churning happens.

### Outer Keccak: Padding

The absorption phase is "chunked", meaning

1. it consumes a chunk of the input bits
2. updates the state (the "sponge")
3. if there are no more input bits to consume, absorption is done
4. else, go to step (1)

The size of this chunk is called the `BitRate`. In order for this to work, the
bit length of the input has to be a multiple of `BitRate`. The padding step
exists to make sure that is true.

The padding rule is `/10*1/`, as in the regex. So for instance, suppose our bit
rate was `10`.

```
input (bytes)           : 00000111 10101100 01001111
input (grouped into 10) : 0000011110 1011000100 1111______
padded input            : 0000011110 1011000100 1111100001
```

The rules are

1. There will *always* be at least two bits of padding added to the end of the
   bitstring
2. The first padding bit will be `1`
3. The last padding bit will be `1`
4. *If* there are intermediate padding bits, they will all be `0`
5. The total number of padding bits added is however many makes the length of
   the resulting bitstring an integer multiple of the `BitRate`

With that in mind:

```erlang
%% https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl#L176-L234

-spec pad(Message, BitRate) -> NewMessage
    when Message    :: bitstring(),
         BitRate    :: pos_integer(),
         NewMessage :: bitstring().
%% @private
%% padding
%% divide the message into r-bit blocks
%%
%% the message ends with 1000...0001
%%
%% sha3 calls this /10*1/ as in the regex
%%
%% Reference: https://en.wikipedia.org/wiki/SHA-3#Padding
%% @end

% note: rem will always return a positive integer because both bit_size
% case when the message bit length is evenly divisible by the bit rate
% in this case we add a whole new r-word
pad(Message, BitRate = _r) when (bit_size(Message) rem BitRate) =:= 0 ->
    % Suppose the BitRate was 8 and we had 0 bits left
    % Input:
    %   Bits: <<>>
    %   Idx1: 12345678
    % Result:
    %   Bits: 10000001
    %   Idx1: 12345678
    % In this case we add a new r-word
    NewRWord   = <<1:1, 0:(BitRate - 2), 1:1>>,
    NewMessage = <<Message/bitstring, NewRWord/bitstring>>,
    NewMessage;
% this is the retarded case, when the bit length of the message is exactly one
% bit less than dividing the BitRate
pad(Message, BitRate = _r) when (bit_size(Message) rem BitRate) =:= (BitRate - 1) ->
    % Suppose the BitRate was 8 and we had 7 bits left
    % Input:
    %   Bits: ABCDEFG
    %   Idx1: 12345678
    % Result:
    %   Bits: ABCDEFG1 00000001
    %   Idx1: 12345678 12345678
    % in this case, we add a 1, (r-1) zeros, and a trailing 1
    NewRWord   = <<1:1, 0:(BitRate - 1), 1:1>>,
    NewMessage = <<Message/bitstring, NewRWord/bitstring>>,
    NewMessage;
% this is the general case, where there are at least 2 bits left in order to
% fill out the r-word
pad(Message, BitRate = _r) ->
    % Suppose the BitRate was 8 and we had 3 bits left
    % Input:
    %   Bits: ABC
    %   Idx1: 12345678
    % Result:
    %   Bits: ABC10001
    %   Idx1: 12345678
    NumberOfMessageBitsInTheLastRWord = bit_size(Message) rem BitRate,
    NumberOfNewBitsNeeded             = BitRate - NumberOfMessageBitsInTheLastRWord,
    NumberOfNewZerosNeeded            = NumberOfNewBitsNeeded - 2,
    NewMessage                        = <<Message/bitstring, 1:1, 0:NumberOfNewZerosNeeded, 1:1>>,
    NewMessage.
```

### Outer Keccak: Absorption phase

As I mentioned above, absorption is "chunked", and the chunk size is the bit
rate.  Absorption comes after padding.  So in our absorb procedure, we can
*assume* the length of the `PaddedMessage` is an integer multiple of `BitRate`.

The procedure is

1.  Consume `BitRate` bits off the input `PaddedMessage` and put these bits
    into `ThisRWord`
2.  Bitwise xor `ThisRWord` against the `Sponge`

    Let's suppose `BitRate = 10` as before

    ```
    PaddedMessage : 0000011110 1011000100 1111100001
    ThisRWord     : 0000011110
    Sponge        : 1111110000 0100011010 1001111010 ... (1600 bits)
    AugRWord      : 0000011110 0000000000 0000000000 ... (1600 bits)
    InnerKekInput : 1111101110 0100011010 1001111010 ... (1600 bits, result of xoring the two previous lines)
    ```

3.  Take the freshly xored sponge and pass it to `inner_keccak/1`
4.  Repeat until you run out of `PaddedMessage` bits.


```erlang
-spec absorb(PaddedMessage, BitRate, Capacity, SpongeAcc) -> WetSponge
    when PaddedMessage :: bitstring(),
         BitRate       :: pos_integer(),
         Capacity      :: pos_integer(),
         SpongeAcc     :: <<_:1600>>,
         WetSponge     :: <<_:1600>>.
%% @private
%% Assumptions:
%%  1. BitRate + Capacity = 1600,
%%  2. BitRate divides the PaddedMessage length (i.e. already have done padding)
%% @end

% can pull off r bits from the start of the message
absorb(PaddedMessageBits, BitRate = _r, Capacity = _c, Sponge) when BitRate =< bit_size(PaddedMessageBits) ->
    <<ThisRWord:BitRate, Rest/bitstring>> = PaddedMessageBits,
    % we bitwise xor the sponge against the r word followed by a bunch of 0s
    <<SpongeInt:1600>> = Sponge,
    <<Foo:1600>>       = <<ThisRWord:BitRate, 0:Capacity>>,
    FInputInt          = SpongeInt bxor Foo,
    FInputBits         = <<FInputInt:1600>>,
    NewSponge          = inner_keccak(FInputBits),
    absorb(Rest, BitRate, Capacity, NewSponge);
% empty string, return the sponge
absorb(<<>>, _r, _c, FinalSponge) ->
    FinalSponge.
```


[german-lecture]: https://www.youtube.com/watch?v=JWskjzgiIa4
[german-lecture-notes]: https://www.crypto-textbook.com/download/Understanding-Cryptography-Keccak.pdf
[nist-standard]: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
[^sponge]: Source for photo: https://www.flickr.com/photos/30478819@N08/46410395345
