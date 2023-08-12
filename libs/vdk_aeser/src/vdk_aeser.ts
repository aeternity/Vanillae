/**
 * Miscellaneous serialization/deserialization functions
 *
 * Refs:
 * 1. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 * 2. https://github.com/aeternity/protocol/blob/master/serializations.md
 *
 * @module
 */


import * as vdk_base58 from './jex_include/local-vdk_base58-0.1.0/dist/vdk_base58.js';
import * as vdk_binary from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';


export {
    pubkey2ak_str,
    add_check_bytes,
    shasha4
}



/**
 * Take a public key as 32 bytes and encode it in AE API ak_... notation
 *
 * Refs:
 * 1. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 */
async function
pubkey2ak_str
    (pubkey_bytes : Uint8Array)
    : Promise<string>
{
    let checked_pubkey_bytes : Uint8Array = await add_check_bytes(pubkey_bytes);
    let b58_str              : string     = vdk_base58.encode(checked_pubkey_bytes);
    return ("ak_" + b58_str);
}



/**
 * Take the double-sha of a byte array and return the first 4 bytes
 *
 * TypeScript equivalent of
 *
 * ```erlang
 * add_check_bytes(Bin) when is_binary(Bin) ->
 *     <<Check:4/binary, _/binary>> = sha256(sha256(Bin)),
 *     <<Bin/binary, Check/binary>>.
 * ```
 *
 * Refs:
 * 1. https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
 * 2. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 * 3. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer/slice
 *
 * is async because everything is retarded.
*/
async function
add_check_bytes
    (pubkey_bytes : Uint8Array)
    : Promise<Uint8Array>
{
    let check_bytes : Uint8Array = await shasha4(pubkey_bytes);
    return vdk_binary.bytes_concat(pubkey_bytes, check_bytes);
}


/**
 * Take the double-sha of a byte array and return the first 4 bytes
 *
 * TypeScript equivalent of
 *
 * ```erlang
 * shasha4(Bin) when is_binary(Bin) ->
 *     <<Check:4/binary, _/binary>> = sha256(sha256(Bin)),
 *     <<Check/binary>>.
 * ```
 *
 * Refs:
 * 1. https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
 * 2. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 * 3. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer/slice
 *
 * is async because everything is retarded.
*/
async function
shasha4
    (pubkey_bytes : Uint8Array)
    : Promise<Uint8Array>
{
    let shad_bytes      : ArrayBuffer = await crypto.subtle.digest("SHA-256", pubkey_bytes);
    let shashad_bytes   : ArrayBuffer = await crypto.subtle.digest("SHA-256", shad_bytes);
    let shashad_bytes_4 : ArrayBuffer = shashad_bytes.slice(0, 4);
    return new Uint8Array(shashad_bytes_4);
}
