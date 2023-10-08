/**
 * Black boxes away all the AE-specific cryptography routines that call other
 * libs
 *
 * Refs:
 * 1. https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L141-L
 * 2. https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L160-L
 * 3. https://github.com/aeternity/Vanillae/blob/f054744bbceb957afa8fbcf31d93055dd277396b/sidekick/src/sidekick.ts#L926-L1015
 *
 * @module
 */


// @ts-ignore FIXME yes blake2b sucks
import * as blake2b    from './jex_include/local-blakejs-1.2.1/dist/blake2b.js';
import * as vdk_binary from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';


export {
    hash_and_salt_msg,
    hash,
    salt_msg,
    btc_varuint_encode
}



/**
 * Quoth https://github.com/aeternity/Vanillae/blob/f054744bbceb957afa8fbcf31d93055dd277396b/sidekick/src/sidekick.ts#L926-L1015
 *
 * > In order to exclude the possibility of someone using this functionality to
 * > trick the user into signing a transaction, the wallet salts and hashes the
 * > message, and *then* signs the salted/hashed message.
 * >
 * > Therefore, naively attempting to verify the signature will not work. You
 * > must apply the same preprocessing steps as the wallet, **THEN** check the
 * > signature against the salted/hashed message.
 * >
 * > ```erlang
 * > -spec hashed_salted_msg(Message) -> HashedSaltedMessage
 * >     when Message             :: binary(),
 * >          HashedSaltedMessage :: binary().
 * > % @doc Salt the message then hash with blake2b. See:
 * > % 1. https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L83-L85
 * > % 2. https://github.com/aeternity/eblake2/blob/60a079f00d72d1bfcc25de8e6996d28f912db3fd/src/eblake2.erl#L23-L25
 * >
 * > hashed_salted_msg(Msg) ->
 * >     {ok, HSMsg} = eblake2:blake2b(32, salted_msg(Msg)),
 * >     HSMsg.
 * >
 * >
 * >
 * > -spec salted_msg(Message) -> SaltedMessage
 * >     when Message       :: binary(),
 * >          SaltedMessage :: binary().
 * > % @doc Salt the message the way Superhero does before signing.
 * > %
 * > % See: https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L171-L175
 * >
 * > salted_msg(Msg) when is_binary(Msg) ->
 * >     P = <<"aeternity Signed Message:\n">>,
 * >     {ok, SP}   = btc_varuint_encode(byte_size(P)),
 * >     {ok, SMsg} = btc_varuint_encode(byte_size(Msg)),
 * >     <<SP/binary,
 * >       P/binary,
 * >       SMsg/binary,
 * >       Msg/binary>>.
 * >
 * >
 * >
 * > -spec btc_varuint_encode(Integer) -> Result
 * >     when Integer :: integer(),
 * >          Result  :: {ok, Encoded :: binary()}
 * >                   | {error, Reason :: term()}.
 * > % @doc Bitcoin varuint encode
 * > %
 * > % See: https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
 * >
 * > btc_varuint_encode(N) when N < 0 ->
 * >     {error, {negative_N, N}};
 * > btc_varuint_encode(N) when N < 16#FD ->
 * >     {ok, <<N>>};
 * > btc_varuint_encode(N) when N =< 16#FFFF ->
 * >     NBytes = eu(N, 2),
 * >     {ok, <<16#FD, NBytes/binary>>};
 * > btc_varuint_encode(N) when N =< 16#FFFF_FFFF ->
 * >     NBytes = eu(N, 4),
 * >     {ok, <<16#FE, NBytes/binary>>};
 * > btc_varuint_encode(N) when N < (2 bsl 64) ->
 * >     NBytes = eu(N, 8),
 * >     {ok, <<16#FF, NBytes/binary>>}.
 * >
 * > % eu = encode unsigned (little endian with a given byte width)
 * > % means add zero bytes to the end as needed
 * > eu(N, Size) ->
 * >     Bytes = binary:encode_unsigned(N, little),
 * >     NExtraZeros = Size - byte_size(Bytes),
 * >     ExtraZeros = << <<0>> || _ <- lists:seq(1, NExtraZeros) >>,
 * >     <<Bytes/binary, ExtraZeros/binary>>.
 * > ```
 */
function
hash_and_salt_msg
    (message_str : string)
    : Uint8Array
{
    let message_bytes : Uint8Array = vdk_binary.encode_utf8(message_str);
    let salted_bytes  : Uint8Array = salt_msg(message_bytes);
    return hash(salted_bytes);
}



/**
 * Blake2 hash of data
 */
function
hash
    (data_bytes : Uint8Array)
    : Uint8Array
{
    // console.error('hash!');
    return blake2b.blake2b(data_bytes,      // bytes to hash
                           undefined,       // key (optional)
                           32);             // resulting byte length
}



/**
 * salt the message
 *
 * ```erlang
 * -spec salted_msg(Message) -> SaltedMessage
 *     when Message       :: binary(),
 *          SaltedMessage :: binary().
 * % @doc Salt the message the way Superhero does before signing.
 * %
 * % See: https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L171-L175
 *
 * salted_msg(Msg) when is_binary(Msg) ->
 *     P = <<"aeternity Signed Message:\n">>,
 *     {ok, SP}   = btc_varuint_encode(byte_size(P)),
 *     {ok, SMsg} = btc_varuint_encode(byte_size(Msg)),
 *     <<SP/binary,
 *       P/binary,
 *       SMsg/binary,
 *       Msg/binary>>.
 * ```
 */
function
salt_msg
    (msg_bytes : Uint8Array)
    : Uint8Array
{
    let prefix_str        : string     = 'aeternity Signed Message:\n';
    let prefix_bytes      : Uint8Array = vdk_binary.encode_utf8(prefix_str);
    let prefix_size_n     : number     = prefix_bytes.byteLength;
    let prefix_size_bytes : Uint8Array = btc_varuint_encode(prefix_size_n);
    let msg_size_n        : number     = msg_bytes.byteLength;
    let msg_size_bytes    : Uint8Array = btc_varuint_encode(msg_size_n);

    return vdk_binary.bytes_concat_arr([prefix_size_bytes,
                                        prefix_bytes,
                                        msg_size_bytes,
                                        msg_bytes]);
}



/**
 * btc varuint encoding
 *
 * function is the following Erlang code translated into TS
 *
 * ```erlang
 * -spec btc_varuint_encode(Integer) -> Result
 *     when Integer :: integer(),
 *          Result  :: {ok, Encoded :: binary()}
 *                   | {error, Reason :: term()}.
 * % @doc Bitcoin varuint encode
 * %
 * % See: https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
 *
 * btc_varuint_encode(N) when N < 0 ->
 *     {error, {negative_N, N}};
 * btc_varuint_encode(N) when N < 16#FD ->
 *     {ok, <<N>>};
 * btc_varuint_encode(N) when N =< 16#FFFF ->
 *     NBytes = eu(N, 2),
 *     {ok, <<16#FD, NBytes/binary>>};
 * btc_varuint_encode(N) when N =< 16#FFFF_FFFF ->
 *     NBytes = eu(N, 4),
 *     {ok, <<16#FE, NBytes/binary>>};
 * btc_varuint_encode(N) when N < (2 bsl 64) ->
 *     NBytes = eu(N, 8),
 *     {ok, <<16#FF, NBytes/binary>>}.
 * ```
 */
function
btc_varuint_encode
    (n : number)
    : Uint8Array
{
    if
    (n < 0) {
        throw new Error('n < 0');
    }
    else if
    (n < 0xFD) {
        return new Uint8Array([n]);
    }
    else if
    (n < 0xFFFF) {
        let prefix  : Uint8Array = new Uint8Array([0xFD]);
        let n_bytes : Uint8Array = eu(n, 2);
        return vdk_binary.bytes_concat(prefix, n_bytes);
    }
    else if
    (n < 0xFFFF_FFFF) {
        let prefix  : Uint8Array = new Uint8Array([0xFE]);
        let n_bytes : Uint8Array = eu(n, 4);
        return vdk_binary.bytes_concat(prefix, n_bytes);
    }
    else {
        let prefix  : Uint8Array = new Uint8Array([0xFF]);
        let n_bytes : Uint8Array = eu(n, 8);
        return vdk_binary.bytes_concat(prefix, n_bytes);
    }
}



/**
 * unsigned integer little endian encoding with a given byte width
 *
 * @internal
 */
function
eu
    (n          : number,
     byte_width : number)
    : Uint8Array
{
    // endianness is byte-level not bit-level
    // 3> binary:encode_unsigned(258, big).
    // <<1,2>>
    // 4> binary:encode_unsigned(258, little).
    // <<2,1>>

    // first encode n as little endian
    let n_bytes         : Uint8Array = vdk_binary.bigint_to_bytes_little(BigInt(n));
    // figure out how much padding we need
    let num_extra_zeros : number     = byte_width - n_bytes.byteLength;
    let extra_zeros     : Uint8Array = vdk_binary.bytes_zeros(num_extra_zeros);

    // in little endian, padding 0s go on the right
    return vdk_binary.bytes_concat(n_bytes, extra_zeros);
}

