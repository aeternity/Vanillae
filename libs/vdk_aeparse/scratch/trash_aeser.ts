/**
 * Miscellaneous serialization/deserialization functions
 *
 * Refs:
 * 1. https://github.com/aeternity/protocol/blob/master/node/api/api_encoding.md
 * 2. https://github.com/aeternity/protocol/blob/master/serializations.md
 * 3. https://github.com/colinhacks/zod/blob/481c9ba1932203777f6fe9497bb2a8a1d33c620e/README.md#basic-usage
 *
 * @module
 */


import * as vdk_base58 from './jex_include/local-vdk_base58-0.1.0/dist/vdk_base58.js';
import * as vdk_base64 from './jex_include/local-vdk_base64-0.1.0/dist/vdk_base64.js';
import * as vdk_binary from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';
import * as vdk_rlp    from './jex_include/local-vdk_rlp-0.1.0/dist/vdk_rlp.js';


export {
    OTAG_SPEND,
    pubkey2ak_str,
    add_check_bytes,
    shasha4,
    unbaseNcheck,
    baseNcheck,
    signed_tx,
    mansplain,
    erlangify_binary
}

export type {
    baseNchecked
}


// constants
let OTAG_SPEND = 12;



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
 * Full list of human readable objects we can return
 */
type mansplained_obj =
        mansplained_spendtx;



/**
 * `payload` will be in Erlang binary notation: either (for instance)
 * `<<"hainana">>` or `<<42, 11, 22, ...>>`
 */
type mansplained_spendtx
    = {otag      : 12,
       stag      : 'SpendTx',
       version   : 1,
       sender    : string,
       recipient : string,
       amount    : BigInt,
       fee       : BigInt,
       ttl       : BigInt,
       nonce     : BigInt,
       payload   : string};



/**
 * Take apart some API-encoded data and show its component parts into
 * "human-readable" format. What that will look like strongly depends on
 * subjective taste on my part.
 */
async function
mansplain
    (api_str : string)
    : Promise< {ok : true,  result : mansplained_obj}
             | {ok : false, error  : string}>
{
    // TODO: I am mentally checked out and am in mental goo quicksand land
    // right now. The next task is taking specifically API strings of spends,
    // and putting them into that human readable format.
    //
    // Probably the task after that is doing so with the various contract
    // things
    //
    // Eventually we want to get everything here covered:
    // https://github.com/aeternity/protocol/blob/master/serializations.md
    //
    // Really only the transaction types... hmm. Ok maybe that's the source of
    // mental goo is that really only we want transactions and not ALL API
    // data.  And there's many levels here. Ok the mental goo needs to just dry
    // and I can't touch it. I'm leaving. Out of mental stamina.
    //
    // ok so make tx_... readable specifically for spends
    // need to warn about GA transactions
    //
    // um like eventually we actually want to return structured data back to
    // the caller. like the thing is the caller wants to make, like he doesn't
    // want strings. like not one big string. the thing with that like for
    // instance he wants to provide a link on a spend to the account of the guy
    // being paid on aescan or whatever. fuck you. you get my point.
    //
    // so maybe mansplain_str is not good, we should like... ok but in b_lib...
    // the thing is we're going to eventually need a way to create that popup
    // with like more structure than just sending it a string... so like I
    // don't know is there a way to create like structured HTML nodes in JS
    // that aren't attached to anything and send them across tabs as
    // messages??? I don't know... probably don't want to know the answer.
    //
    // anyway i think mansplain_str should be deleted and moved to b_lib...
    //
    // ok so but our task for now is to create the nice pretty structured data
    // for spends, and that's really not that hard.
    //
    // ok let's make the task smaller rather than bigger
    //
    // i will do that next time I sit down... am mentally checked out. am going
    // to go lift heavy things because goo brain.
    let x = await unbaseNcheck(api_str);

    if
    (!(x.check_passed))
        return {ok    : false,
                error : 'checksum failed on unbaseNcheck'};
    else
        return await mansplain_bytes(x.bytes);
}



type rlpdata      = Uint8Array | Array<rlpdata>;
type decode_data  = {decoded_data : rlpdata,
                     remainder    : Uint8Array};



/**
 * Take RLP encoded bytes and mansplain them
 *
 * @internal
 */
async function
mansplain_bytes
    (data_bytes : Uint8Array)
    : Promise< {ok     : true,
                result : mansplained_obj}
             | {ok        : false,
                error     : string,
                miscinfo? : any}>
{

    let x : decode_data = vdk_rlp.decode(data_bytes);
    // make sure we decoded everything
    if
    (0 !== x.remainder.length)
        return {ok       : false,
                error    : 'mansplain_bytes: trailing data on rlp decode',
                miscinfo : {data_bytes    : data_bytes,
                            decoded_data  : x.decoded_data}};
    // if we did indeed decode everything, next make sure we decoded an array
    // and not a binary
    else if
    (x.decoded_data instanceof Uint8Array)
        return {ok       : false,
                error    : 'mansplain_bytes: rlp decoded a binary, was expecting to decode an array',
                miscinfo : {data_bytes   : data_bytes,
                            decoded_data : x.decoded_data}};
    // alright this language is simply too insane to sanity check
    // everything... I see why Metin likes that parser-assertion library. Zod
    // I think it was called.  This is the type of situation where it's super
    // useful.
    else {
        // so fields are
        // [otag, version, ...fields]
        // @ts-ignore I know it's an array
        let fields : Array<Uint8Array> = x.decoded_data;

        let otag       : number            = fields[0][0];
        let vsn        : number            = fields[1][0];
        let fields_raw : Array<Uint8Array> = fields.slice(2);
        // dispatch based on the otag
        return await mansplain_dispatch(otag, vsn, fields_raw);
    }
}



/**
 * Figure out which type of thing this is, and send it to the appropriate function
 *
 * Also makes sure that version/tag is supported
 *
 * @internal
 */
async function
mansplain_dispatch
    (otag       : number,
     vsn        : number,
     fields_raw : Array<rlpdata>)
    : Promise< {ok : true,  result : mansplained_obj}
             | {ok : false, error  : string}>
{
    if
    (is_spend(otag, vsn))
        return await mansplain_spendtx_fields(fields_raw);
    else
        return {ok              : false,
                error           : 'mansplain_dispatch: error while mansplaining Aeternity object: either invalid object/version tag combo or (more likely) not yet implemented',
                otag            : otag,
                vsn             : vsn,
                fields_raw      : fields_raw,
                ref             : 'https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#table-of-object-tags'};
}



/**
 * Return true if it is a supported spendtx
 *
 * - object 
 *
 * In general we might support multiple versions
 *
 * @internal
 */
function
is_spend
    (otag       : number,
     vsn        : number,
     fields_raw : Array<rlpdata>)
    : boolean
{
    return ((otag === OTAG_SPEND) && (vsn === 1));
}



/**
 * Mansplain RLP-decoded spendtx fields
 *
 * Hmm... You know, zod is a really really good idea
 *
 * I think on the net it is a positive
 *
 * The issues in this case are
 * 1. I don't know how to package it with jex
 * 2. This is inviting AWS problems where I end up writing my entire codebase
 * up against some library that I don't trust, and it's very very hard to
 * divorce myself from that---frankly it would be a full rewrite---if I ever
 * want to do that.
 *
 * @internal
 */
async function
mansplain_spendtx_fields
    (fields : Array<Uint8Array>)
    : {ok : true,  result : mansplained_spendtx}
    | {ok : false, error  : string}
{
    // spend fields
    // Ref:  https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#spend-transaction
    // at this point we know
    return {otag      : OTAG_SPEND,
            stag      : 'SpendTx',
            vsn       : 1,
            sender    : await id2apistr(fields[0]),            // <sender>    :: id()
            recipient : await id2apistr(fields[1]),            // <recipient> :: id()
            amount    : vdk_binary.bytes_to_bigint(fields[2]), // <amount>    :: int()
            fee       : vdk_binary.bytes_to_bigint(fields[3]), // <fee>       :: int()
            ttl       : vdk_binary.bytes_to_bigint(fields[4]), // <ttl>       :: int()
            nonce     : vdk_binary.bytes_to_bigint(fields[5]), // <nonce>     :: int()
            payload   : erlangify_binary(fields[6])};          // <payload>   :: binary()
}



/**
 * Print a binary in Erlang notation
 *
 * @internal
 */
function
erlangify_binary
    (bytes : Uint8Array)
    : string
{
    // so stupid but I love it
    function bytestr(n: number): string {
        return n + '';
    }

    // mmm
    // special case if
    // ok the issue is fucking interspersing commas
    // so annoying
    // ok so let's just make code that's obviously correct and don't try to overthink it
    // 0 bytes = empty string
    if
    (0 === bytes.length)
        return '<<>>'
    // 1 byte = just the first byte
    else if
    (1 === bytes.length) {
        return '<<' + bytestr(bytes[0]) + '>>';
    }
    // at least 2 bytes = grab first byte
    else {
        let first_byte : number = bytes[0];
        let result_acc : string = '<<'  + bytestr(first_byte);

        // put commas at the beginning of every one starting with the second
        // byte
        for (let this_i0 = 1;
                 this_i0 < bytes.length;
                 ++this_i0)
        {
            let this_byte : number = bytes[i0];

            result_acc += ', ';
            result_acc += bytestr(this_byte);
        }

        // add trailing >>
        result_acc += '>>';

        return result_acc;
    }
}