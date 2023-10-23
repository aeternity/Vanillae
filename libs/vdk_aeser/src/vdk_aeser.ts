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
import * as vdk_base64 from './jex_include/local-vdk_base64-0.1.0/dist/vdk_base64.js';
import * as vdk_binary from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';
import * as vdk_rlp from './jex_include/local-vdk_rlp-0.1.0/dist/vdk_rlp.js';


export {
    pubkey2ak_str,
    add_check_bytes,
    shasha4,
    unbaseNcheck,
    baseNcheck,
    signed_tx,
    mansplain,
    mansplain_str
}

export type {
    baseNchecked
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
    (data_bytes : Uint8Array)
    : Promise<Uint8Array>
{
    let check_bytes : Uint8Array = await shasha4(data_bytes);
    return vdk_binary.bytes_concat(data_bytes, check_bytes);
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



/**
 * Result of decoding a baseNCheck encoded string
 */
type baseNchecked
    = {bytes        : Uint8Array,
       check_passed : boolean}


/**
 * take a baseNcheck (N = 58, 64) encoded string and get out the resulting bytes
 *
 * returns boolean
 */
async function
unbaseNcheck
    (tx_str : string)
    : Promise<baseNchecked>
{
    // tx_... ak_... etc
    let prefix   : string = tx_str.slice(0, 2);
    let baseNstr : string = tx_str.slice(3);

    // branch for base58 or base64 based on prefix
    // table:
    // https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md
    switch (prefix) {
        case 'ak': return await unbase58check(baseNstr); // Account pubkey
        case 'ba': return await unbase64check(baseNstr); // Byte array
        case 'bf': return await unbase58check(baseNstr); // Block Proof of Fraud hash
        case 'bs': return await unbase58check(baseNstr); // Block State hash
        case 'bx': return await unbase58check(baseNstr); // Block transaction hash
        case 'cb': return await unbase64check(baseNstr); // Contract byte array
        case 'ch': return await unbase58check(baseNstr); // Channel
        case 'cm': return await unbase58check(baseNstr); // Commitment
        case 'ct': return await unbase58check(baseNstr); // Contract pubkey
        case 'kh': return await unbase58check(baseNstr); // Key block hash
        case 'mh': return await unbase58check(baseNstr); // Micro block hash
        case 'nm': return await unbase58check(baseNstr); // Name
        case 'ok': return await unbase58check(baseNstr); // Oracle pubkey
        case 'oq': return await unbase58check(baseNstr); // Oracle query id
        case 'or': return await unbase64check(baseNstr); // Oracle response
        case 'ov': return await unbase64check(baseNstr); // Oracle query
        case 'pi': return await unbase64check(baseNstr); // Proof of Inclusion
        case 'pp': return await unbase58check(baseNstr); // Peer pubkey
        case 'sg': return await unbase58check(baseNstr); // Signature
        case 'ss': return await unbase64check(baseNstr); // State trees
        case 'cs': return await unbase64check(baseNstr); // Contract calls state tree
        case 'ck': return await unbase64check(baseNstr); // Contract state key
        case 'cv': return await unbase64check(baseNstr); // Contract state value
        case 'st': return await unbase64check(baseNstr); // State
        case 'th': return await unbase58check(baseNstr); // Transaction hash
        case 'tx': return await unbase64check(baseNstr); // Transaction
        default  : throw new Error('unknown prefix: ' + prefix);
    }
}

async function
unbase64check
    (baseNstr : string)
    : Promise<baseNchecked>
{
    let bytes               : Uint8Array = vdk_base64.decode(baseNstr);
    // ah ok here is the bug
    let bytes_len           : number     = bytes.byteLength;
    let data_bytes_len      : number     = bytes_len - 4;
    let data_bytes          : Uint8Array = bytes.slice(0, data_bytes_len);
    let check_bytes         : Uint8Array = bytes.slice(data_bytes_len);
    let correct_check_bytes : Uint8Array = await shasha4(data_bytes);
    let check_passed        : boolean    = vdk_binary.bytes_eq(correct_check_bytes, check_bytes);
    return {bytes        : data_bytes,
            check_passed : check_passed};
}


async function
unbase58check
    (baseNstr : string)
    : Promise<baseNchecked>
{
    let bytes               : Uint8Array = vdk_base58.decode(baseNstr);
    let bytes_len           : number     = bytes.byteLength;
    let data_bytes_len      : number     = bytes_len - 4;
    let data_bytes          : Uint8Array = bytes.slice(0, data_bytes_len);
    let check_bytes         : Uint8Array = bytes.slice(data_bytes_len);
    let correct_check_bytes : Uint8Array = await shasha4(data_bytes);
    let check_passed        : boolean    = vdk_binary.bytes_eq(correct_check_bytes, check_bytes);
    return {bytes        : data_bytes,
            check_passed : check_passed};
}



/**
 * take bytes and encode using baseNcheck (N = 58, 64) according to the prefix
 */
async function
baseNcheck
    (prefix     : string,
     data_bytes : Uint8Array)
    : Promise<string>
{
    // tx_... ak_... etc

    // branch for base58 or base64 based on prefix
    // table:
    // https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md
    switch (prefix) {
        case 'ak': return await base58check(prefix, data_bytes); // Account pubkey
        case 'ba': return await base64check(prefix, data_bytes); // Byte array
        case 'bf': return await base58check(prefix, data_bytes); // Block Proof of Fraud hash
        case 'bs': return await base58check(prefix, data_bytes); // Block State hash
        case 'bx': return await base58check(prefix, data_bytes); // Block transaction hash
        case 'cb': return await base64check(prefix, data_bytes); // Contract byte array
        case 'ch': return await base58check(prefix, data_bytes); // Channel
        case 'cm': return await base58check(prefix, data_bytes); // Commitment
        case 'ct': return await base58check(prefix, data_bytes); // Contract pubkey
        case 'kh': return await base58check(prefix, data_bytes); // Key block hash
        case 'mh': return await base58check(prefix, data_bytes); // Micro block hash
        case 'nm': return await base58check(prefix, data_bytes); // Name
        case 'ok': return await base58check(prefix, data_bytes); // Oracle pubkey
        case 'oq': return await base58check(prefix, data_bytes); // Oracle query id
        case 'or': return await base64check(prefix, data_bytes); // Oracle response
        case 'ov': return await base64check(prefix, data_bytes); // Oracle query
        case 'pi': return await base64check(prefix, data_bytes); // Proof of Inclusion
        case 'pp': return await base58check(prefix, data_bytes); // Peer pubkey
        case 'sg': return await base58check(prefix, data_bytes); // Signature
        case 'ss': return await base64check(prefix, data_bytes); // State trees
        case 'cs': return await base64check(prefix, data_bytes); // Contract calls state tree
        case 'ck': return await base64check(prefix, data_bytes); // Contract state key
        case 'cv': return await base64check(prefix, data_bytes); // Contract state value
        case 'st': return await base64check(prefix, data_bytes); // State
        case 'th': return await base58check(prefix, data_bytes); // Transaction hash
        case 'tx': return await base64check(prefix, data_bytes); // Transaction
        default  : throw new Error('unknown prefix: ' + prefix);
    }
}

async function
base64check
    (prefix     : string,
     data_bytes : Uint8Array)
    : Promise<string>
{
    let full_bytes  : Uint8Array = await add_check_bytes(data_bytes);
    let full_str    : string     = vdk_base64.encode(full_bytes);
    return prefix + '_' + full_str;
}


async function
base58check
    (prefix     : string,
     data_bytes : Uint8Array)
    : Promise<string>
{
    let full_bytes  : Uint8Array = await add_check_bytes(data_bytes);
    let full_str    : string     = vdk_base58.encode(full_bytes);
    return prefix + '_' + full_str;
}


/**
 * RLP-encode signed tx (signatures and tx are both the BINARY representations)
 *
 * See https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#signed-transaction
 */
function
signed_tx
    (signatures : Array<Uint8Array>,
     tx         : Uint8Array)
    : Uint8Array
{
    // tag for signed tx
    let tag_bytes = vdk_rlp.encode_uint(11);
    // not sure what version number should be but guessing 1
    let vsn_bytes = vdk_rlp.encode_uint(1);
    // result is [tag, vsn, signatures, tx]
    return vdk_rlp.encode([tag_bytes, vsn_bytes, signatures, tx]);
}



/**
 * Take apart some API-encoded data and return a string that explains it to the
 * user.
 *
 * Mostly for the "confirm transaction" popup
 */
async function
mansplain_str
    (api_str : string)
    : Promise<string>
{
    let mansplained_obj : object = await mansplain(api_str)

    return JSON.stringify(mansplained_obj, undefined, 4);
}



/**
 * Take apart some API-encoded data and show its component parts
 *
 * For debugging purposes
 */
async function
mansplain
    (api_str : string)
    : Promise<object>
{
    let prefix : string

    let x = await unbaseNcheck(api_str);

    let check_passed : boolean    = x.check_passed;
    let data_bytes   : Uint8Array = x.bytes;

    let mansplained_data : object = mansplain_bytes(data_bytes);

    return {check_passed : check_passed,
            data         : mansplained_data};
}



/**
 * Take RLP encoded bytes and mansplain them
 */
function
mansplain_bytes
    (data_bytes : Uint8Array)
    : object
{
    return vdk_rlp.decode(data_bytes);
}
