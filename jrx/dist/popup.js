"use strict";
/**
 * This is the script for the popup window
 */
//import {default as nacl} from './nacl.js';
main();
/**
 * runs whenever the popup opens
 */
async function main() {
    // detect button action
    document.getElementById('mk-detectable').onclick = detuctable;
    document.getElementById('generate').onclick = generate_keypair;
    // delete keypairs
    // browser.storage.local.clear();
    await relist_keypairs();
}
/**
 * Tells the content script to spam the event bus with detection messages
 */
async function detuctable() {
    let ati = await active_tab_id();
    // @ts-ignore browser api unknown
    browser.tabs.sendMessage(ati, // active tab
    'mk-detectable'); // message
    // @ts-ignore disabled exists on this
    document.getElementById('mk-detectable').disabled = true;
    // FIXME: update button with detect info
}
/**
 * gets the id of the current tab
 */
async function active_tab_id() {
    // @ts-ignore browser api unkown
    let active_tabs = await browser.tabs.query({ active: true, currentWindow: true });
    return active_tabs[0].id;
}
/**
 * clear the list of keypairs and relist
 *
 * clearing is so this can be a re-entry call
 */
async function relist_keypairs() {
    // delete all list items
    pfizer(document.getElementById('keypairs'));
    // look for keypairs then relist
    // @ts-ignore browser api
    let obj = await browser.storage.local.get('keypairs');
    await handle_keypairs(obj);
}
/**
 * kill all child nodes in the dom
 */
function pfizer(elt) {
    while (elt.firstChild) {
        elt.removeChild(elt.firstChild);
    }
}
/**
 * This function is called in the init phase (after the user clicks the popup)
 *
 * If we have keypairs, put them in the keypairs element in the popup window
 *
 * Example call objects:
 *
 * Case of no keypairs:
 *
 * ```js
 * obj = {}
 * ```
 *
 * Case of a named keypair
 *
 * ```js
 * obj = {
 *     keypairs: [
 *         {
 *             name: string,
 *             publicKey: uint8array
 *             secretKey: uint8array
 *         },
 *         {
 *             name: string,
 *             publicKey: uint8array
 *             secretKey: uint8array
 *         }
 *     ]
 * }
 * ```
 */
async function handle_keypairs(obj) {
    logln('handle_keypairs');
    // branch on if there are no keypairs
    // no 'keypairs' key
    if (!!!obj.keypairs) {
        no_keypairs();
    }
    // 'keypairs' key but points to empty array
    else if (0 === obj.keypairs.length) {
        no_keypairs();
    }
    else {
        await ls_keypairs(obj.keypairs);
    }
}
/**
 * function called if there are no keypairs
 */
function no_keypairs() {
    logln('no keypairs!');
    document.getElementById('no-keypairs').hidden = false;
}
//       publicKey : Uint8Array,
//       secretKey : Uint8Array};
/**
 * This function is called when the user clicks the button to generate a keypair
 */
async function generate_keypair() {
    logln('generating a keypair');
    // @ts-ignore namespace nacl
    let keypair = nacl.sign.keyPair();
    // @ts-ignore changing type
    keypair.name = "Untitled Keypair 1";
    // fixme: ADD keypair (factor out into separate function)
    // @ts-ignore cannot find namespace browser
    browser.storage.local.set({ 'keypairs': [keypair] });
    await relist_keypairs();
}
/**
 * This function is called in the case where there is at least one keypair, and
 * it renders the list of keypairs
 */
async function ls_keypairs(keypairs) {
    logln('keypairs!');
    console.log('keypairs: ', keypairs);
    // make keypair list visible
    let keypairs_ul = document.getElementById('keypairs');
    keypairs_ul.hidden = false;
    // render each keypair
    for (let kp of keypairs) {
        console.log(kp);
        await render_keypair(keypairs_ul, kp);
    }
    //let keypairs_json = JSON.stringify(keypairs, undefined, 4);
    //document.getElementById('keypairs').innerHTML = keypairs_json;
}
/**
 * This renders a single keypair item in the list of keypairs
 */
async function render_keypair(parent_ul, nk) {
    let name = nk.name;
    let publicKey = nk.publicKey;
    // create the element
    // <li></li>
    let li = document.createElement('li');
    // <li></li> -> <li>{name}</li>
    li.innerHTML += name;
    // <li>{name}</li> -> <li>{name}<br></li>
    li.appendChild(document.createElement('br'));
    // <code></code>
    let code = document.createElement('code');
    let pubkey_str = await publicKey_bytes_to_str(publicKey);
    // <code></code> -> <code>ak_...</code>
    code.innerHTML += pubkey_str;
    // <li>...</li> -> <li>...<code>...</code></li>
    li.appendChild(code);
    // <ul></ul> -> <ul><li>...</li></ul>
    parent_ul.appendChild(li);
}
/**
 * take the publicKey bytes and output a string of the form ak_...
 */
async function publicKey_bytes_to_str(publicKey_bytes) {
    // From https://github.com/aeternity/protocol/blob/c007deeac4a01e401238412801ac7084ac72d60e/node/api/api_encoding.md
    //
    // ```erlang
    // add_check_bytes(Bin) when is_binary(Bin) ->
    //     <<Check:4/binary, _/binary>> = sha256(sha256(Bin)),
    //     <<Bin/binary, Check/binary>>.
    // ```
    let bytes_to_encode = await shasha4_concat(publicKey_bytes);
    let b58_str = encode(bytes_to_encode);
    return 'ak_' + b58_str;
}
/**
 * take the first 4 bytes from sha256(sha256(inputBytes)) and concatenate it onto input, return that
 *
 * From https://github.com/aeternity/protocol/blob/c007deeac4a01e401238412801ac7084ac72d60e/node/api/api_encoding.md
 *
 * ```erlang
 * add_check_bytes(Bin) when is_binary(Bin) ->
 *     <<Check:4/binary, _/binary>> = sha256(sha256(Bin)),
 *     <<Bin/binary, Check/binary>>.
 * ```
 */
async function shasha4_concat(input_bytes) {
    let shasha4_bytes = await shasha4(input_bytes);
    let input_bytes_length = input_bytes.byteLength;
    // this is so dumb
    // make blank array
    let return_bytes = new Uint8Array(input_bytes_length + 4);
    // copy input bytes into it
    for (let input_bytes_idx0 = 0; input_bytes_idx0 < input_bytes_length; input_bytes_idx0++) {
        return_bytes[input_bytes_idx0] = input_bytes[input_bytes_idx0];
    }
    // copy sha bytes
    for (let shasha4_bytes_idx0 = 0; shasha4_bytes_idx0 < 4; shasha4_bytes_idx0++) {
        // if shasha4_bytes_idx0 = 0, then we want to start at input_bytes_length
        let return_bytes_idx0 = input_bytes_length + shasha4_bytes_idx0;
        return_bytes[return_bytes_idx0] = shasha4_bytes[shasha4_bytes_idx0];
    }
    return return_bytes;
}
/**
 * the first 4 bytes of the sha256 of the sha256 of the input
 *
 * returns a Uint8Array
 */
async function shasha4(input_bytes) {
    let x = await crypto.subtle.digest('SHA-256', input_bytes);
    let y = await crypto.subtle.digest('SHA-256', x);
    let z = y.slice(0, 4);
    return new Uint8Array(z);
}
/**
 * Console.log the message and put it in the log thing
 */
function logln(message) {
    console.log(message);
    document.getElementById('log').innerHTML += message;
    document.getElementById('log').innerHTML += '\n';
}
/**
 * Base58 encoding/decoding
 */
//=============================================================================
// ENCODING
//=============================================================================
/**
 * Encode a Uint8Array into base58
 */
function encode(binary) {
    let num_leading_zeros = nlz(binary);
    let rest = binary.slice(num_leading_zeros);
    let ones = encode_zeros(num_leading_zeros);
    let rest_b58 = encode_rest(rest);
    let result = ones + rest_b58;
    return result;
}
/**
 * count the number of leading zeros in a uint8array
 *
 * @internal
 */
function nlz(bytes) {
    let n = 0;
    for (let this_byte of bytes) {
        if (0 === this_byte) {
            n++;
        }
        else {
            break;
        }
    }
    return n;
}
/**
 * Generate a bunch of '1's for however many leading zeros there are
 *
 * @internal
 */
function encode_zeros(how_many) {
    let ones = '';
    for (let i = 1; i <= how_many; i++) {
        ones += '1';
    }
    return ones;
}
/**
 * Encode a Uint8Array that has no leading zeros
 *
 * @internal
 */
function encode_rest(bytes) {
    let bytes_bignum = bytes_to_bigint(bytes);
    let result = bignum_to_base58(bytes_bignum);
    return result;
}
/**
 * Convert a bytestring to a bignum
 *
 * @internal
 */
function bytes_to_bigint(bytes) {
    let acc_bigint = 0n;
    for (let this_byte of bytes) {
        acc_bigint <<= 8n;
        acc_bigint += BigInt(this_byte);
    }
    return acc_bigint;
}
/**
 * Convert a BigInt to Base58
 *
 * @internal
 */
function bignum_to_base58(q) {
    let s = '';
    while (q !== 0n) {
        let this_n = q % 58n;
        q /= 58n;
        let this_b58_char = bigint_to_char(this_n);
        s = this_b58_char + s;
    }
    return s;
}
//=============================================================================
// DECODING
//=============================================================================
/**
 * Decode a Base58 string into a Uint8Array
 */
function decode(base58) {
    let num_leading_ones = nlo(base58);
    let rest = base58.slice(num_leading_ones);
    let zeros = decode_ones(num_leading_ones);
    let rest_arr = decode_rest(rest);
    let pre_result = zeros.concat(rest_arr);
    return new Uint8Array(pre_result);
}
/**
 * count the number of leading 1 characters in a uint8array
 *
 * @internal
 */
function nlo(base58) {
    let n = 0;
    for (let this_char of base58) {
        if ('1' === this_char) {
            n++;
        }
        else {
            break;
        }
    }
    return n;
}
/**
 * Generate a bunch of '0's for however many leading ones there are
 *
 * @internal
 */
function decode_ones(how_many) {
    let zeros = [];
    for (let i = 1; i <= how_many; i++) {
        zeros.push(0);
    }
    return zeros;
}
/**
 * Decode a string that has no leading 1s
 *
 * @internal
 */
function decode_rest(base58) {
    let result_bignum = base58_to_bigint(base58);
    let result = bigint_to_base256(result_bignum);
    return result;
}
/**
 * Convert a base58 string to a bignum
 *
 * @internal
 */
function base58_to_bigint(base58) {
    let acc_bigint = 0n;
    for (let this_char of base58) {
        acc_bigint *= 58n;
        acc_bigint += char_to_bigint(this_char);
    }
    return acc_bigint;
}
/**
 * convert a bignum into a byte array
 *
 * @end
 */
function bigint_to_base256(q) {
    let arr_reverse = [];
    while (q !== 0n) {
        let r = Number(q % 256n);
        q /= 256n;
        arr_reverse.push(r);
    }
    arr_reverse.reverse();
    return arr_reverse;
}
//=============================================================================
// TRANSLATION TABLES
//=============================================================================
/**
 * Base58 integer -> character conversion table
 *
 * @internal
 */
function bigint_to_char(n) {
    switch (n) {
        case 0n: return '1';
        case 1n: return '2';
        case 2n: return '3';
        case 3n: return '4';
        case 4n: return '5';
        case 5n: return '6';
        case 6n: return '7';
        case 7n: return '8';
        case 8n: return '9';
        case 9n: return 'A';
        case 10n: return 'B';
        case 11n: return 'C';
        case 12n: return 'D';
        case 13n: return 'E';
        case 14n: return 'F';
        case 15n: return 'G';
        case 16n: return 'H';
        case 17n: return 'J';
        case 18n: return 'K';
        case 19n: return 'L';
        case 20n: return 'M';
        case 21n: return 'N';
        case 22n: return 'P';
        case 23n: return 'Q';
        case 24n: return 'R';
        case 25n: return 'S';
        case 26n: return 'T';
        case 27n: return 'U';
        case 28n: return 'V';
        case 29n: return 'W';
        case 30n: return 'X';
        case 31n: return 'Y';
        case 32n: return 'Z';
        case 33n: return 'a';
        case 34n: return 'b';
        case 35n: return 'c';
        case 36n: return 'd';
        case 37n: return 'e';
        case 38n: return 'f';
        case 39n: return 'g';
        case 40n: return 'h';
        case 41n: return 'i';
        case 42n: return 'j';
        case 43n: return 'k';
        case 44n: return 'm';
        case 45n: return 'n';
        case 46n: return 'o';
        case 47n: return 'p';
        case 48n: return 'q';
        case 49n: return 'r';
        case 50n: return 's';
        case 51n: return 't';
        case 52n: return 'u';
        case 53n: return 'v';
        case 54n: return 'w';
        case 55n: return 'x';
        case 56n: return 'y';
        case 57n: return 'z';
        default:
            throw new Error('invalid base58 bigint: ' + n);
    }
}
/**
 * Base58 character -> integer conversion table
 *
 * @internal
 */
function char_to_bigint(s) {
    switch (s) {
        case '1': return 0n;
        case '2': return 1n;
        case '3': return 2n;
        case '4': return 3n;
        case '5': return 4n;
        case '6': return 5n;
        case '7': return 6n;
        case '8': return 7n;
        case '9': return 8n;
        case 'A': return 9n;
        case 'B': return 10n;
        case 'C': return 11n;
        case 'D': return 12n;
        case 'E': return 13n;
        case 'F': return 14n;
        case 'G': return 15n;
        case 'H': return 16n;
        case 'J': return 17n;
        case 'K': return 18n;
        case 'L': return 19n;
        case 'M': return 20n;
        case 'N': return 21n;
        case 'P': return 22n;
        case 'Q': return 23n;
        case 'R': return 24n;
        case 'S': return 25n;
        case 'T': return 26n;
        case 'U': return 27n;
        case 'V': return 28n;
        case 'W': return 29n;
        case 'X': return 30n;
        case 'Y': return 31n;
        case 'Z': return 32n;
        case 'a': return 33n;
        case 'b': return 34n;
        case 'c': return 35n;
        case 'd': return 36n;
        case 'e': return 37n;
        case 'f': return 38n;
        case 'g': return 39n;
        case 'h': return 40n;
        case 'i': return 41n;
        case 'j': return 42n;
        case 'k': return 43n;
        case 'm': return 44n;
        case 'n': return 45n;
        case 'o': return 46n;
        case 'p': return 47n;
        case 'q': return 48n;
        case 'r': return 49n;
        case 's': return 50n;
        case 't': return 51n;
        case 'u': return 52n;
        case 'v': return 53n;
        case 'w': return 54n;
        case 'x': return 55n;
        case 'y': return 56n;
        case 'z': return 57n;
        default:
            throw new Error('invalid base58 char: ' + s);
    }
}
//# sourceMappingURL=popup.js.map