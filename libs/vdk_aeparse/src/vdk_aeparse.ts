/**
 * Miscellaneous parsing/validation and serialization/deserialization functions
 *
 * Refs:
 * 1. https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/node/api/api_encoding.md
 * 2. https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md
 * 3. https://github.com/colinhacks/zod/blob/481c9ba1932203777f6fe9497bb2a8a1d33c620e/README.md#basic-usage
 *
 * @module
 */


import * as vdk_base58 from './jex_include/local-vdk_base58-0.1.0/dist/vdk_base58.js';
import * as vdk_base64 from './jex_include/local-vdk_base64-0.1.0/dist/vdk_base64.js';
import * as vdk_binary from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';
import * as vdk_rlp    from './jex_include/local-vdk_rlp-0.1.0/dist/vdk_rlp.js';
import * as z          from './zod_3_22_4.js';


export {
    // AE object numerical tags
    AE_OBJ_NTAG_SPEND,
    // AE object string tags
    AE_OBJ_STAG_SPEND,
    // AE ID numerical tags
    AE_ID_NTAG_ACCOUNT,
    AE_ID_NTAG_NAME,
    AE_ID_NTAG_COMMITMENT,
    AE_ID_NTAG_ORACLE,
    AE_ID_NTAG_CONTRACT,
    AE_ID_NTAG_CHANNEL,
    // AE ID string tags
    AE_ID_STAG_ACCOUNT,
    AE_ID_STAG_NAME,
    AE_ID_STAG_COMMITMENT,
    AE_ID_STAG_ORACLE,
    AE_ID_STAG_CONTRACT,
    AE_ID_STAG_CHANNEL,
    // zod parsers for AE idents
    AEz_id,
    AEz_id_account,
    AEz_id_name,
    AEz_id_commitment,
    AEz_id_oracle,
    AEz_id_contract,
    AEz_id_channel,
    AEz_idhash,
    AEz_SpendTx,
    // zod parsers for Erlang types
    ERLz_non_neg_integer,
    ERLz_binary
}

export type {
}


// Let's just start with spends
// A spend has... mmmm the correct abstraction is views
// ok
//
// > Objects in æternity are encoded as lists of fields, where the two first
// > fields describe the object type and the object version.
// >
// > [ <tag>, <version>, <field1>, <field2>...]
//
// Fields of Spend:
// Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#spend-transaction
//
// [ <sender>    :: id()
// , <recipient> :: id()
// , <amount>    :: int()
// , <fee>       :: int()
// , <ttl>       :: int()
// , <nonce>     :: int()
// , <payload>   :: binary()
// ]
//
// id(): https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
//
// OK what I've described here are parsers at the end of the parsing chain
// So zod is super helpful in giving me a lexicon of concepts that help me
// break my problem down into smaller problems, in a better way. It's clearing
// that mental mud. And it's allowing me a super compact and very expressive
// language for doing this.
//
// man I don't think we can do without this. it's too good. fuck
//
// anyway, I think what I'm going to do is make this into my data validation
// library

// entire chain
// string -> tx_... -> base58check -> rlp decoded -> each field


/**
 * This is the entire parser from an API string to an AEz_SpendTx
 *
 * za = asynchronous parser
 */
let AEza_SpendTx_apistr
    = z.string()
       .startsWith('tx_')
       // the ... of tx_... is a length which is a multiple of 4
       // my head hurts... i need to give this a day or two and come back to this
       .transform(isBase64Checked)



/**
 * Returns `true` if this is a `xy_...` string where the `...` is base64
 * encoded and the check bytes are added properly
 *
 * @internal
 */
async function isBase64Str(str: string, zodContext)
{
    // `tx_...` -> `...`
    let base64_chars : string     = str.slice(3);
    // problem here is sunk cost which is that base64 just crashes if it gets invalid input
    // and the thing with zod is it wants to consume all the errors
    let bytes        : Uint8Array = vdk_base64.decode(base64_chars);
}



// Object numerical tags (incomplete)
// Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#table-of-object-tags
let AE_OBJ_NTAG_SPEND : number = 12;



// Object string tags (incomplete)
// Source: I made it up
let AE_OBJ_STAG_SPEND : number = 'SpendTx';



// AE ID numerical tags
// Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
let AE_ID_NTAG_ACCOUNT    : number = 1;
let AE_ID_NTAG_NAME       : number = 2;
let AE_ID_NTAG_COMMITMENT : number = 3;
let AE_ID_NTAG_ORACLE     : number = 4;
let AE_ID_NTAG_CONTRACT   : number = 5;
let AE_ID_NTAG_CHANNEL    : number = 6;



// AE ID string tags
let AE_ID_STAG_ACCOUNT    : number = 'account';
let AE_ID_STAG_NAME       : number = 'name';
let AE_ID_STAG_COMMITMENT : number = 'commitment';
let AE_ID_STAG_ORACLE     : number = 'oracle';
let AE_ID_STAG_CONTRACT   : number = 'contract';
let AE_ID_STAG_CHANNEL    : number = 'channel';




// parsers
// ok the organizing principle here is that the data types here are those which
// are most useful to a programmer
// the programmer-user can get different views if he wants

/**
 * Zod parser for AE ids
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id
    = z.discriminatedUnion(id_ntag,
                           [AEz_id_account,
                            AEz_id_name,
                            AEz_id_oracle,
                            AEz_id_contract,
                            AEz_id_channel]);



/**
 * AE id zod parser, account case
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id_account
    = z.object({id_ntag : z.literal(AE_ID_NTAG_ACCOUNT),
                id_stag : z.literal(AE_ID_STAG_ACCOUNT),
                id_hash : AEz_idhash});



/**
 * AE id zod parser, name case
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id_name
    = z.object({id_ntag : z.literal(AE_ID_NTAG_NAME),
                id_stag : z.literal(AE_ID_STAG_NAME),
                id_hash : AEz_idhash});



/**
 * AE id zod parser, oracle case
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id_oracle
    = z.object({id_ntag : z.literal(AE_ID_NTAG_ORACLE),
                id_stag : z.literal(AE_ID_STAG_ORACLE),
                id_hash : AEz_idhash});



/**
 * AE id zod parser, contract case
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id_contract
    = z.object({id_ntag : z.literal(AE_ID_NTAG_CONTRACT),
                id_stag : z.literal(AE_ID_STAG_CONTRACT),
                id_hash : AEz_idhash});



/**
 * AE id zod parser, channel case
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_id_channel
    = z.object({id_ntag : z.literal(AE_ID_NTAG_CHANNEL),
                id_stag : z.literal(AE_ID_STAG_CHANNEL),
                id_hash : AEz_idhash});




/**
 * The actual id (public key, etc)
 *
 * 32 bytes
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#the-id-type
 */
let AEz_idhash
    = ERLz_binary.length(32);



/**
 * Zod parser for Spends
 *
 * Ref: https://github.com/aeternity/protocol/blob/fd179822fc70241e79cbef7636625cf344a08109/serializations.md#spend-transaction
 */
let AEz_SpendTx
    = z.object({stag      : z.literal(AE_OBJECT_STAG_SPEND),
                ntag      : z.literal(AE_OBJECT_NTAG_SPEND),
                vsn       : z.literal(1),
                sender    : AEz_id,
                recipient : AEz_id,
                amount    : ERLz_non_neg_integer,
                fee       : ERLz_non_neg_integer,
                ttl       : ERLz_non_neg_integer,
                nonce     : ERLz_non_neg_integer,
                payload   : ERLz_binary});



// copying names from erlang

/**
 * Erlang `binary()` type
 *
 * not a uint8array because apparently that's not supported?
 * Uint8Array api sucks anyway who cares
 */
let ERLz_binary
    = z.array(ERLz_byte);



/**
 * Not necessarily an erlang type, but an integer between 0 and 255
 *
 * `byte()` sort of seems like it would be an Erlang type but I don't know that it is.
 */
let ERLz_byte
    = z.number()
        .int()
        .gte(0)
        .lte(255);



/**
 * Alias for `ERLZ_binary`
 */
let ERLz_bytes
    = ERLz_binary;



/**
 * Erlang `non_neg_integer()` type
 *
 * arbitrary byte-length non-negative integer
 */
let ERLz_non_neg_integer
    = z.bigint()
       .nonnegative();
