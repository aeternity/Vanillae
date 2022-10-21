/**
 * Base58 encoding/decoding
 */

export {
    encode
    // decode
}



//=============================================================================
// ENCODING
//=============================================================================

/**
 * Encode a Uint8Array into base58
 */
function
encode(binary : Uint8Array): string {
    let num_leading_zeros : number     = nlz(binary);
    let rest              : Uint8Array = binary.slice(num_leading_zeros);
    let ones              : string     = encode_zeros(num_leading_zeros);
    let rest_b58          : string     = encode_rest(rest);
    let result            : string     = ones + rest_b58;
    return result;
}



/**
 * count the number of leading zeros in a uint8array
 *
 * @internal
 */
function
nlz(bytes: Uint8Array): number {
    let n = 0;
    for (let this_byte of bytes) {
        if (0 === this_byte)
            n++;
        else
            break;
    }
    return n;
}


/**
 * Generate a bunch of '1's for however many leading zeros there are
 *
 * @internal
 */
function
encode_zeros(how_many : number): string {
    let ones : string = '';
    for (let i  = 1;
             i <= how_many;
             i++)
    {
        ones += '1';
    }

    return ones;
}



/**
 * Encode a Uint8Array that has no leading zeros
 *
 * @internal
 */
function
encode_rest(bytes : Uint8Array): string {
    let bytes_bignum : bigint = bytes_to_bigint(bytes);
    let result       : string = bignum_to_base58(bytes_bignum);
    return result;
}



/**
 * Convert a bytestring to a bignum
 *
 * @internal
 */
function
bytes_to_bigint(bytes: Uint8Array): bigint {
    let acc_bigint : bigint = 0n;
    for(let this_byte of bytes) {
        acc_bigint <<= 8n;
        acc_bigint  += BigInt(this_byte);
    }
    return acc_bigint;
}



/**
 * Convert a BigInt to Base58
 *
 * @internal
 */
function
bignum_to_base58(q: bigint) {
    let s = '';
    while (q !== 0n) {
        let this_n        : bigint = q % 58n;
        q /= 58n;

        let this_b58_char : string = bigint_to_char(this_n);
        s = this_b58_char + s;
    }
    return s;
}



//=============================================================================
// TRANSLATION TABLES
//=============================================================================

function
bigint_to_char(n: bigint) {
    switch(n) {
        case  0n: return '1';
        case  1n: return '2';
        case  2n: return '3';
        case  3n: return '4';
        case  4n: return '5';
        case  5n: return '6';
        case  6n: return '7';
        case  7n: return '8';
        case  8n: return '9';
        case  9n: return 'A';
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
            throw new Error('invalid base58 bigint: ' + n)
    }
}
