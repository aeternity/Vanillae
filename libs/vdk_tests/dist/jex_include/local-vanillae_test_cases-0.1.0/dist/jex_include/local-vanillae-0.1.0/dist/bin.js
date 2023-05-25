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
function bytes_to_bigint(bytes) {
    let n = 0n;
    for (let b of bytes) {
        // move first, then add
        // otherwise it ends on a move
        // imperative languages are for losers
        n <<= 8n;
        n += BigInt(b);
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
function bigint_to_bytes(q) {
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
//# sourceMappingURL=bin.js.map