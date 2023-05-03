/**
 * Miscellaneous binary utility functions
 *
 * @module
 */

export {
    bytes_to_bigint,
    bigint_to_bytes,
    concat,
    strong_rand_bytes
};



/**
 * Concatenate two arrays
 */
function
concat
    (arr1 : Uint8Array,
     arr2 : Uint8Array)
    : Uint8Array
{
    let len1             : number     = arr1.length;
    let len2             : number     = arr2.length;
    let arr1_idx0_offset : number     = 0;
    let arr2_idx0_offset : number     = len1;
    let result_len       : number     = len1 + len2;
    let result           : Uint8Array = new Uint8Array(result_len);
    // copy first array into result
    for (let arr1_idx0 = 0;
             arr1_idx0 < len1;
             arr1_idx0++)
    {
        // no offset here
        let result_idx0 : number = arr1_idx0 + arr1_idx0_offset;
        result[result_idx0] = arr1[arr1_idx0];
    }
    // copy second array into result
    for (let arr2_idx0 = 0;
             arr2_idx0 < len2;
             arr2_idx0++)
    {
        // offset by the length of the first array
        let result_idx0 : number = arr2_idx0 + arr2_idx0_offset;
        result[result_idx0] = arr2[arr2_idx0];
    }
    return result;
}



/**
 * Cryptographically random bytes
 */
function
strong_rand_bytes
    (how_many : number)
    : Uint8Array
{
    let arr = new Uint8Array(how_many);
    Crypto.getRandomValues(arr);
    return arr;
}



/**
 * Convert a byte array to a bigint
 *
 * Equivalent to `binary:decode_unsigned/1` from Erlang
 */
function
bytes_to_bigint
    (bytes: Uint8Array)
    : bigint
{
    let n : bigint = 0n;
    for (let b of bytes) {
        // move first, then add
        // otherwise it ends on a move
        // imperative languages are for losers
        n <<= 8n;
        n  += BigInt(b);
    }
    return n;
}



/**
 * Convert a bigint to a byte array
 *
 * Equivalent to `binary:encode_unsigned/1` from Erlang
 *
 * Requires input to be positive
 */
function
bigint_to_bytes
    (q: bigint)
    : Uint8Array
{
    if (q < 0n) {
        throw new Error('q < 0n: ' + q);
    }

    let arr_reverse = [];
    while (q > 0n) {
        let r = Number(q % 256n);
        q /= 256n;
        arr_reverse.push(r);
    }
    arr_reverse.reverse();
    return new Uint8Array(arr_reverse);
}



/**
 * Oh no, bitstrings in a language that only has bytestrings
 *
 * By convention these are `Uint8Array`s with byte length `ceil(bit_length /
 * 8)`, and all trailing bits are zero.
 */
type bits =
    {bit_length : number,
     bytes      : Uint8Array};



/**
 * Get a bitstring of a given length where every value is 0.
 */
function
zeros
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);
    for (let i0 = 0;
             i0 < byte_length;
             i0++)
    {
        result[i0] = 0;
    }
    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get a bitstring of a given length where every value is 1.
 */
function
ones
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);

    // fill everything except the last byte with 255s
    for (let i0 = 0;
             i0 < (byte_length - 1);
             i0++)
    {
        result[i0] = 255;
    }

    // alright so the last byte
    // ok so the number of leading 0s is
    // 8 - (bit_length % 8)
    let num_trailing_zero_bits : number =  8 - (bit_length % 8);
    // the trailing byte is 255 << that
    // e.g. 3 trailing 0s
    // 1111_1111 -> 1111_1000
    let last_byte      : number = 255 << num_trailing_zero_bits;
    let last_byte_idx0 : number = byte_length - 1;
    result[last_byte_idx0] = last_byte;

    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get the bit at a given 0-index
 */
function
bit_i0th
    (bit_idx0  : number,
     bits      : bits)
    : number
{
    // first task is figuring out what byte we're at
    // for instance if we want bit 27
    // 3*8 = 24 =< 27 < 4*8
    // so it's Math.floor(bit_idx0 / 8)
    let byte_idx0 : number = Math.floor(bit_idx0 / 8);
    // let's fetch the byte and work with that
    let the_byte  : number = bits.bytes[byte_idx0];

    // ok so let's go with 27 again
    // 27 = 3 mod 8
    // so we bitshift right by (8 - 3)
    // and then take the remainder dividing by 2
    // --B-_---- -> ----_---B -> 0000_000B
    let bsr : number = 8 - (bit_idx0 % 8);
    return (the_byte >> bsr) % 2;
}
