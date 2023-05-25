/**
 * Binary utilities
 *
 * @module
 */
export { bytes_to_bigint, bigint_to_bytes };
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
