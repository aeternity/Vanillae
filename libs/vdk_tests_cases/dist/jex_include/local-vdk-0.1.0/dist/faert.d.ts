/**
 * FÆRT: Fast Æternity Recovery Text
 *
 * Reference: https://gitlab.com/zxq9/passgas/-/blob/83607fedb08be5dfd03210e331f9c76125bb3467/fullofbeans
 *
 * @module
 */
export { encode, decode, byte_of_word, check_word, check_byte, words };
import * as safe from './safe.js';
/**
 * Encode a bytestring into FÆRT
 */
declare function encode(bytes: Uint8Array): string;
/**
 * Decode a FAERT string into bytes
 */
declare function decode(faert: string): safe.Safe<Uint8Array, string>;
/**
 * given a word, find its index
 */
declare function byte_of_word(word: string): safe.Safe<number, string>;
/**
 * compute the check word of an array
 */
declare function check_word(bytes: Uint8Array): string;
/**
 * given an array, compute the xor of all the bytes in the array
 */
declare function check_byte(bytes: Uint8Array): number;
declare let words: string[];
