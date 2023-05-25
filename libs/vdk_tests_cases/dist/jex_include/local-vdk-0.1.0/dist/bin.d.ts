/**
 * Miscellaneous binary utility functions
 *
 * @module
 */
export { bytes_to_bigint, bigint_to_bytes, concat, strong_rand_bytes };
/**
 * Concatenate two arrays
 */
declare function concat(arr1: Uint8Array, arr2: Uint8Array): Uint8Array;
/**
 * Cryptographically random bytes
 */
declare function strong_rand_bytes(how_many: number): Uint8Array;
/**
 * Convert a byte array to a bigint
 *
 * Equivalent to `binary:decode_unsigned/1` from Erlang
 */
declare function bytes_to_bigint(bytes: Uint8Array): bigint;
/**
 * Convert a bigint to a byte array
 *
 * Equivalent to `binary:encode_unsigned/1` from Erlang
 *
 * Requires input to be positive
 */
declare function bigint_to_bytes(q: bigint): Uint8Array;
