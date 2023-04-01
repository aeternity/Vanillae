# Keccak, SHA-3, and SHAKE-N algorithms explained, with code in Erlang

Keccak is a hashing algorithm used for the SHA-3 standard. [The
standard][nist-standard] is semi-readable math clownery. Hashing algorithms by
nature have to be complicated and somewhat obfuscated. But, at the end of the
day, all Keccak does is take in some input bits, process them in a
deterministic way, and hand you output bits.

The goal of this document is just to explain what the processing steps are as
straightforwardly as possible.

Keccak is the "general case", and then SHA-3 and SHAKE-128 and so on are
special cases of Keccak.

## tldr

- [Erlang code (clear)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek.erl)
- [Erlang code (fast)](https://github.com/pharpend/kek/blob/8a8a655a80c26ae32763cc25f1e0df8ab0653c82/kek_fast.erl)
- [erlang-sha3 library (uses fast version of kek)](https://github.com/zxq9/erlang-sha3/blob/63193654e3c05d8031300ffcd52092f75e8b5c2f/src/sha3.erl#L85-L112)

#### References

1. [Helpful lecture](https://www.youtube.com/watch?v=JWskjzgiIa4)
2. [Notes for that lecture](https://www.crypto-textbook.com/download/Understanding-Cryptography-Keccak.pdf)
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

Note that this pitfall only applies to the SHA-3 fixed-length hashing
functions. These produce a fixed-length (say, 512 bit) hash.

The SHAKE-N algorithms are sort of "arbitrary length SHA-3". So instead of
producing 512 bits, you tell it how long the output length should be.

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
are just rewrites in front of Keccak.


[nist-standard]: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
