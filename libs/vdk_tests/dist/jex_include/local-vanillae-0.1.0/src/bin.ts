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
    (new Crypto()).getRandomValues(arr);
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
 * Get an uninitialized bitstring
 *
 * @internal
 */
function
bits_null
    (bit_length : number)
    : bits
{
    let byte_length : number     = Math.ceil(bit_length / 8);
    let result      : Uint8Array = new Uint8Array(byte_length);
    return {bit_length : bit_length,
            bytes      : result};
}



/**
 * Get a bitstring of a given length where every value is 0.
 */
function
bits_zeros
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
bits_ones
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
bits_i0th
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



/**
 * Concatenate two bitstrings
 */
function
bits_concat
    (bits1 : bits,
     bits2 : bits)
    : bits
{
    let result_bit_length : number     = bits1.bit_length + bits2.bit_length;
    let bytes1            : Uint8Array = bits1.bytes;
    let bytes2            : Uint8Array = bits2.bytes;
    // using zeros here because of our xor trick in a minute
    let result_bits       : bits       = bits_zeros(result_bit_length);
    let result_bytes      : Uint8Array = result_bits.bytes;

    // alright so
    // we can start by copying the first bytes into result bytes
    for (let bytes1_idx0 = 0;
             bytes1_idx0 < bytes1.length;
             bytes1_idx0++)
    {
       result_bytes[bytes1_idx0] = bytes1[bytes1_idx0];
    }

    // next
    // we need to calculate the left-shift offset
    // this will be 8 - (bytes1.bit_length % 8)
    let num_trailing_zeros_in_first_array : number = 8 - (bits1.bit_length % 8);
    // so
    // bytes1: ABCD_EF00
    // bytes2: GH12_3000
    // result: ABCD_EFGH 1230_0000
    // ah ok, so we need to for each byte in the second array
    // take the first however many bits, xor it with the existing byte
    // then take the last however many bits and place them into the next byte
    // this is super confusing but
    // ABCD_EF00
    //           GH12_3456
    // operation:
    //     ABCD_EF00
    // xor 0000_00GH
    //   = ABCD_EFGH 1234_5600
    //
    // then on the next iteration
    // 1234_5600
    //          abcd_efgh
    // ->
    // 1234_56ab cdef_gh00
    //
    // ah so there's a pattern
    // however many trailing 0s there are in the first array
    // say there's 2
    // we take the first 2 bits of the upcoming byte
    // xor that against the current byte
    // take the last 6 bits of the upcoming byte
    // set the next byte to that
    //
    // have to think about edge behavior
    // this is ripe for off-by-1 errors
    // but i think the general idea is right
    //
    // so we start the iteration
    // on the last byte of the first array
    let last_byte_of_first_array_idx0            : number = bytes1.length - 1;
    // and we end
    // on the second-to-last-byte of the result array
    let second_to_last_byte_of_result_array_idx0 : number = result_bytes.length - 2;
    // the reason we do that is because we're doing this is because we are
    // going along, xoring against the current byte and then setting the next
    // byte
    //
    // ok so
    for (let this_result_byte_idx0  = last_byte_of_first_array_idx0;
             this_result_byte_idx0 <= second_to_last_byte_of_result_array_idx0;
             this_result_byte_idx0++)
    {
       let this_result_byte : number = result_bytes[this_result_byte_idx0];

       // ok here we need to fish out the relevant byte of the second array
       // gaaah
       // so this will be 0 at the start of the loop
       let relevant_byte_of_second_array_idx0 : number = this_result_byte_idx0 - last_byte_of_first_array_idx0;
       let relevant_byte_of_second_array      : number = bytes2[relevant_byte_of_second_array_idx0];

       // ok so let's fish out the leading digits
       // the number of leading digits is the number of trailing 0s in the first array
       let num_leading_digits  : number = num_trailing_zeros_in_first_array;
       let num_trailing_digits : number = 8 - num_leading_digits;

       // suppose there are 2 leading digits and 6 trailing digits
       // ABCD_EFGH
       // leading digits are
       // ABCD_EFGH >> 6 = 0000_00AB
       // trailing digits are
       // (ABCD_EFGH << 2) % 255 = CDEF_GH00
       let leading_digits  : number = relevant_byte_of_second_array >> num_trailing_digits;
       let trailing_digits : number = (relevant_byte_of_second_array << num_leading_digits) % 255;

       // xor the current byte against the leading digits
       let new_this_result_byte : number = this_result_byte ^ leading_digits;
       result_bytes[this_result_byte_idx0] = new_this_result_byte;

       // set the next byte to the trailing digits
       result_bytes[this_result_byte_idx0 + 1] = trailing_digits;
    }

     // i think we're done
     return {bit_length : result_bit_length,
             bytes      : result_bytes};
}
