/**
 * Humanization/dehumanization of data
 *
 * This is similar to serialization/deserialization, but not quite the same thing
 *
 * This is for taking "api-encoded" data, and pulling it apart and seeing what
 * is in it. See example below.
 *
 * This is not exhaustive.
 *
 * Reference: https://github.com/aeternity/protocol/blob/master/serializations.md
 *
 * ## General type rules
 *
 * ```
 * ERLANG TYPE     |   JS TYPE
 * -------------------------------
 * integer         |   bigint
 * list            |   Array
 * binary          |   Uint8Array
 * ```
 *
 * # Example
 *
 * We start with the string `tx_+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==`.
 *
 * We can tell by the `tx_` prefix that this represents transaction data of
 * some sort. But the rest of the data is totally opaque. The task of this
 * module is to "humanize" that `tx_...` string and show what data is contained
 * in the rest of it.
 *
 * The remainder of the string is a base64-encoded bytestring
 *
 * ```erlang
 * 3> io:format("~tw~n", [base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>)]).
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,118,68,68,244,48,85,218,93,85,120,164,154,55,90,250,24,220,161,1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,89,203,191,216,108,88,186,7,3,234,139,205,10,184,167,108,10,134,15,38,245,97,200,0,0,9,135,104,97,105,110,97,110,97,3,182,66,215>>
 * ```
 *
 * That bytestring contains data encoded using Ethereum's RLP codec. Luckily, I
 * wrote an RLP decoder.  RLP has two types of data: binaries, and
 * arbitrary-depth (possibly empty) lists of binaries.
 *
 * ```erlang
 * -type decoded_data() :: binary() | [decoded_data()].
 *
 * -spec decode(RLP) -> {Data, Rest}
 *     when RLP  :: binary(),
 *          Data :: decoded_data(),
 *          Rest :: binary().
 * ```
 *
 * ```erlang
 * 2> rlp:decode(base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>)).
 * {[<<"\f">>,
 *   <<1>>,
 *   <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *     118,68,68,244,48,85,218,93,85,...>>,
 *   <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *     89,203,191,216,108,88,186,...>>,
 *   <<"\n">>,
 *   <<15,38,245,97,200,0>>,
 *   <<0>>,
 *   <<"\t">>,<<"hainana">>],
 *  <<3,182,66,215>>}
 * ```
 *
 * As expected, we get back the return tuple `{Data, Rest}`. `Rest` is the double-sha256 of the beginning
 *
 * ```erlang
 * 3> X = base64:decode(<<"+FgMAaEByWN+RgDnqzvC5n/GQOgjdkRE9DBV2l1VeKSaN1r6GNyhAXtm5sMFBwg25Ol5IFI9w+pZy7/YbFi6BwPqi80KuKdsCoYPJvVhyAAACYdoYWluYW5hA7ZC1w==">>).
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,
 *   127,198,64,232,35,118,68,68,244,48,85,218,93,...>>
 * 4> SizeX = byte_size(X).
 * 94
 * 6> <<RLPEncodedData:(SizeX - 4)/binary, Hash/binary>> = X.
 * <<248,88,12,1,161,1,201,99,126,70,0,231,171,59,194,230,
 *   127,198,64,232,35,118,68,68,244,48,85,218,93,...>>
 * 10> <<Check:4/binary, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, RLPEncodedData)).
 * <<3,182,66,215,195,99,112,99,25,7,84,31,151,188,149,81,
 *   189,184,82,207,164,68,128,43,11,174,236,59,77,...>>
 * 11> Hash.
 * <<3,182,66,215>>
 * 12> Check.
 * <<3,182,66,215>>
 * ```
 *
 * What we're really interested in is `Data`
 *
 * ```erlang
 * 14> {Data, _} = rlp:decode(X).
 * {[<<"\f">>,
 *   <<1>>,
 *   <<1,201,99,126,70,0,231,171,59,194,230,127,198,64,232,35,
 *     118,68,68,244,48,85,218,93,85,...>>,
 *   <<1,123,102,230,195,5,7,8,54,228,233,121,32,82,61,195,234,
 *     89,203,191,216,108,88,186,...>>,
 *   <<"\n">>,
 *   <<15,38,245,97,200,0>>,
 *   <<0>>,
 *   <<"\t">>,<<"hainana">>],
 *  <<3,182,66,215>>}
 * ```
 *
 * `Data` is a list. The first field `<<"\f">>` is meant to be an integer which
 * tells us what type of data this is.
 *
 * ```erlang
 * 16> $\f.
 * 12
 * ```
 *
 * If we look at our table
 * (https://github.com/aeternity/protocol/blob/master/serializations.md#table-of-object-tags),
 * we see that a value of `12` is a spend transaction.
 *
 * The second field `<<1>>` tells us the "version" of the field orderings,
 * which we can ignore for now.
 *
 * The remaining fields are the fields of a spend transaction (https://github.com/aeternity/protocol/blob/master/serializations.md#spend-transaction)
 *
 * ```
 * [ <sender>    :: id()        % <<1,201,99,126,...>    "=" "ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE"
 * , <recipient> :: id()        % <<1,123,102,230,...>   "=" "ak_wM8yFU8eSETXU7VSN48HMDmevGoCMiuveQZgkPuRn1nTiRqyv"
 * , <amount>    :: int()       % <<"\n">>               "=" 10
 * , <fee>       :: int()       % <<15,38,245,97,200,0>> "=" 16_660_000_000_000
 * , <ttl>       :: int()       % <<0>>                  "=" 0
 * , <nonce>     :: int()       % <<"\t">>               "=" 9
 * , <payload>   :: binary()    % <<"hainana">>          "=" "hainana"
 * ]
 * ```
 *
 * Our task here is to be able to pull apart the "tx_..." string into its fields.
 *
 * Need to think about this
 *
 * @module
 */

export {
    OTAG_SIGNED_TX,
    OTAG_SPEND_TX,
    OTAG_CONTRACT_CREATE_TX,
    OTAG_CONTRACT_CALL_TX,
    IDTAG_ACCOUNT,
    IDTAG_NAME,
    IDTAG_CONTRACT,
    id,
    SignedTx,
    SpendTx,
    ContractCreateTx,
    ContractCallTx,
    decode_tx
};

import * as b64 from './b64.js'
import * as rlp from './rlp.js'

const OTAG_SIGNED_TX          = 11n;
const OTAG_SPEND_TX           = 12n;
const OTAG_CONTRACT_CREATE_TX = 42n;
const OTAG_CONTRACT_CALL_TX   = 43n;

type otag = 11n | 12n | 42n | 43n;

const IDTAG_ACCOUNT  = 1n;
const IDTAG_NAME     = 2n;
const IDTAG_CONTRACT = 5n;

type idtag = 1n | 2n | 5n;


type id =
    {tag  : idtag,
     hash : Uint8Array};

type SignedTx =
    {signatures  : Array<Uint8Array>,
     transaction : Uint8Array};

type SpendTx =
    {sender    : id,
     recipient : id,
     amount    : bigint,
     fee       : bigint,
     ttl       : bigint,
     nonce     : bigint,
     payload   : Uint8Array};

type ContractCreateTx =
    {owner      : id,
     nonce      : bigint,
     code       : Uint8Array,
     ct_version : bigint,
     fee        : bigint,
     ttl        : bigint,
     deposit    : bigint,
     amount     : bigint,
     gas        : bigint,
     gas_price  : bigint,
     call_data  : Uint8Array};

type ContractCallTx =
    {caller      : id,
     nonce       : bigint,
     contract    : id,
     abi_version : bigint,
     fee         : bigint,
     ttl         : bigint,
     amount      : bigint,
     gas         : bigint,
     gas_price   : bigint,
     call_data   : Uint8Array};

type tx = SignedTx | SpendTx | ContractCreateTx | ContractCallTx;

type decoded_tx =
    {tag     : otag,
     version : Uint8Array,
     tx      : tx};

/**
 * Decode a `tx_Base64` string
 */
function
decode_tx(tx_str : string): decoded_tx {
    let base64_stuff   : string                  = tx_str.slice(3);                                                // tx_[...] -> [...]
    let stuff          : Uint8Array              = b64.decode(base64_stuff);                                        // <<Bin/binary, DoubleSha:4>>
    let rlp_stuff      : Uint8Array              = stuff.slice(0, stuff.length - 4);                                 // <<Bin/binary>>
    let decoded_datas : Array<rlp.decoded_data> = rlp.decode(rlp_stuff).decoded_data as Array<rlp.decoded_data>;   // decoded_data : list(rlp.decoded_data() :: binary() | list(decoded_data()))
    // tag, vsn
    let tag_bytes     : Uint8Array              = decoded_datas[0] as Uint8Array; // [tag, vsn, fields] -> tag
    let tag           : bigint                  = bytes_to_bigint(tag_bytes);     // <<Tag:(byte_size(TagBytes))>> = TagBytes
    let vsn           : Uint8Array              = decoded_datas[1] as Uint8Array;
    // tx fields
    let tx_fields     : Array<rlp.decoded_data> = decoded_datas.slice(2);
    let tx            : tx                      = decode_fields(tag, tx_fields);
    return {tag: tag as otag, version: vsn, tx: tx};
}



/**
 * Convert a byte array to a bigint
 *
 * @internal
 */
function
bytes_to_bigint(bytes: Uint8Array): bigint {
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
 * Decode a transaction given the raw fields
 *
 * @internal
 */
function
decode_fields(tag: bigint, fields: Array<rlp.decoded_data>): tx {
    switch (tag) {
        case 11n: return decode_fields_SignedTx(fields);
        case 12n: return decode_fields_SpendTx(fields);
        case 42n: return decode_fields_ContractCreateTx(fields);
        case 43n: return decode_fields_ContractCallTx(fields);
        default : throw new Error("invalid object tag: " + tag);
    }
    console.log('fields: ', fields);
    throw new Error("nyi");
}


/**
 * Decode a SignedTx
 *
 * @internal
 */
function
decode_fields_SignedTx(fields: Array<rlp.decoded_data>): SignedTx {
    throw new Error('nyi');
}


/**
 * Decode a SpendTx
 *
 * @internal
 */
function
decode_fields_SpendTx(fields: Array<rlp.decoded_data>): SpendTx {
    // [<sender>    :: id(),
    //  <recipient> :: id(),
    //  <amount>    :: int(),
    //  <fee>       :: int(),
    //  <ttl>       :: int(),
    //  <nonce>     :: int(),
    //  <payload>   :: binary()]
    let sender    : id         = decode_id(fields[0] as Uint8Array);
    let recipient : id         = decode_id(fields[1] as Uint8Array);
    let amount    : bigint     = bytes_to_bigint(fields[2] as Uint8Array);
    let fee       : bigint     = bytes_to_bigint(fields[3] as Uint8Array);
    let ttl       : bigint     = bytes_to_bigint(fields[4] as Uint8Array);
    let nonce     : bigint     = bytes_to_bigint(fields[5] as Uint8Array);
    let payload   : Uint8Array = fields[6] as Uint8Array;
    return {sender    : sender,
            recipient : recipient,
            amount    : amount,
            fee       : fee,
            ttl       : ttl,
            nonce     : nonce,
            payload   : payload};

}

function
decode_fields_ContractCreateTx(fields: Array<rlp.decoded_data>): ContractCreateTx {
    throw new Error('nyi');
}

function
decode_fields_ContractCallTx(fields: Array<rlp.decoded_data>): ContractCallTx {
    throw new Error('nyi');
}

function
decode_id(id: Uint8Array): id {
    let idtag : idtag = BigInt(id[0]) as idtag;
    return {tag: idtag, hash: id.slice(1)};
}
