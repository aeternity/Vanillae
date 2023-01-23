/**
 * This is the script for the popup window
 */
/**
 * runs whenever the popup opens
 */
declare function main(): Promise<void>;
/**
 * Tells the content script to spam the event bus with detection messages
 */
declare function detuctable(): Promise<void>;
/**
 * gets the id of the current tab
 */
declare function active_tab_id(): Promise<number>;
/**
 * clear the list of keypairs and relist
 *
 * clearing is so this can be a re-entry call
 */
declare function relist_keypairs(): Promise<void>;
/**
 * kill all child nodes in the dom
 */
declare function pfizer(elt: HTMLElement): void;
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
declare function handle_keypairs(obj: {
    keypairs?: Array<named_keypair>;
}): Promise<void>;
/**
 * function called if there are no keypairs
 */
declare function no_keypairs(): void;
declare type keypair = {
    publicKey: Uint8Array;
    secretKey: Uint8Array;
};
declare type named_keypair = keypair & {
    name: string;
};
/**
 * This function is called when the user clicks the button to generate a keypair
 */
declare function generate_keypair(): Promise<void>;
/**
 * This function is called in the case where there is at least one keypair, and
 * it renders the list of keypairs
 */
declare function ls_keypairs(keypairs: Array<named_keypair>): Promise<void>;
/**
 * This renders a single keypair item in the list of keypairs
 */
declare function render_keypair(parent_ul: HTMLElement, nk: named_keypair): Promise<void>;
/**
 * take the publicKey bytes and output a string of the form ak_...
 */
declare function publicKey_bytes_to_str(publicKey_bytes: Uint8Array): Promise<string>;
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
declare function shasha4_concat(input_bytes: Uint8Array): Promise<Uint8Array>;
/**
 * the first 4 bytes of the sha256 of the sha256 of the input
 *
 * returns a Uint8Array
 */
declare function shasha4(input_bytes: Uint8Array): Promise<Uint8Array>;
/**
 * Console.log the message and put it in the log thing
 */
declare function logln(message: string): void;
/**
 * Base58 encoding/decoding
 */
/**
 * Encode a Uint8Array into base58
 */
declare function encode(binary: Uint8Array): string;
/**
 * count the number of leading zeros in a uint8array
 *
 * @internal
 */
declare function nlz(bytes: Uint8Array): number;
/**
 * Generate a bunch of '1's for however many leading zeros there are
 *
 * @internal
 */
declare function encode_zeros(how_many: number): string;
/**
 * Encode a Uint8Array that has no leading zeros
 *
 * @internal
 */
declare function encode_rest(bytes: Uint8Array): string;
/**
 * Convert a bytestring to a bignum
 *
 * @internal
 */
declare function bytes_to_bigint(bytes: Uint8Array): bigint;
/**
 * Convert a BigInt to Base58
 *
 * @internal
 */
declare function bignum_to_base58(q: bigint): string;
/**
 * Decode a Base58 string into a Uint8Array
 */
declare function decode(base58: string): Uint8Array;
/**
 * count the number of leading 1 characters in a uint8array
 *
 * @internal
 */
declare function nlo(base58: string): number;
/**
 * Generate a bunch of '0's for however many leading ones there are
 *
 * @internal
 */
declare function decode_ones(how_many: number): Array<number>;
/**
 * Decode a string that has no leading 1s
 *
 * @internal
 */
declare function decode_rest(base58: string): Array<number>;
/**
 * Convert a base58 string to a bignum
 *
 * @internal
 */
declare function base58_to_bigint(base58: string): bigint;
/**
 * convert a bignum into a byte array
 *
 * @end
 */
declare function bigint_to_base256(q: bigint): Array<number>;
/**
 * Base58 integer -> character conversion table
 *
 * @internal
 */
declare function bigint_to_char(n: bigint): string;
/**
 * Base58 character -> integer conversion table
 *
 * @internal
 */
declare function char_to_bigint(s: string): bigint;
