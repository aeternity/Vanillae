import * as cases from './cases.js';

import * as b64 from './jex_include/local-vanillae-0.1.0/dist/base64.js';
import * as b58 from './jex_include/local-vanillae-0.1.0/dist/base58.js';

function uint8arr_eq(a: Uint8Array, b: Uint8Array) {
    // first test if the length
    if (a.length !== b.length) {
        return false;
    }

    // alright so they have the same length
    for (let i = 0; i < a.length; i++) {
        // return false if a[i] != b[i]
        if (a[i] !== b[i]) {
            return false;
        }
    }

    return true;
}


function b64_tests(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    let b64_pre   : HTMLElement = document.getElementById('b64-assershins');
    // @ts-ignore ts can't prove to itself that the element exists
    let b64_casen : HTMLElement = document.getElementById('b64-current-case');
    let case_n    : number      = 1;

    for(let this_case of cases.base64) {
        // i love this type error
        // can't set the inner html to a number
        // but a string plus a number is totally cool
        b64_casen.innerHTML = '' + case_n;
        case_n++;

        let {encoded, decoded} = this_case;
        let my_encoded         = b64.encode(decoded);
        let my_decoded         = b64.decode(encoded);

        let encodes_correctly = (encoded === my_encoded);
        let decodes_correctly = uint8arr_eq(decoded, my_decoded);

        if (!encodes_correctly) {
            b64_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: encode\n'                 +
                '===================================\n' +
                'decoded : ' + decoded + '\n'           +
                'expected: ' + encoded + '\n'           +
                'actual  : ' + my_encoded + '\n\n'      ;
        }

        if (!decodes_correctly) {
            b64_pre.innerHTML += 
                '===================================\n' +
                'FAILED CASE: decode\n'                 +
                '===================================\n' +
                'encoded : ' + encoded + '\n'           +
                'expected: ' + decoded + '\n'           +
                'actual  : ' + my_decoded + '\n\n'      ;
        }
    }
}


function b58_tests(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    let b58_pre   : HTMLElement = document.getElementById('b58-assershins');
    // @ts-ignore ts can't prove to itself that the element exists
    let b58_casen : HTMLElement = document.getElementById('b58-current-case');
    let case_n    : number      = 1;

    for(let this_case of cases.base58) {
        // i love this type error
        // can't set the inner html to a number
        // but a string plus a number is totally cool
        b58_casen.innerHTML = '' + case_n;
        case_n++;

        let {encoded, decoded} = this_case;
        let my_encoded         = b58.encode(decoded);
        let my_decoded         = b58.decode(encoded);

        let encodes_correctly = (encoded === my_encoded);
        let decodes_correctly = uint8arr_eq(decoded, my_decoded);

        if (!encodes_correctly) {
            b58_pre.innerHTML +=
                '===================================\n' +
                'FAILED CASE: encode\n'                 +
                '===================================\n' +
                'decoded : ' + decoded + '\n'           +
                'expected: ' + encoded + '\n'           +
                'actual  : ' + my_encoded + '\n\n'      ;
        }

        if (!decodes_correctly) {
            b58_pre.innerHTML += 
                '===================================\n' +
                'FAILED CASE: decode\n'                 +
                '===================================\n' +
                'encoded : ' + encoded + '\n'           +
                'expected: ' + decoded + '\n'           +
                'actual  : ' + my_decoded + '\n\n'      ;
        }
    }
}

function main(): void {
    // @ts-ignore ts can't prove to itself that the element exists
    document.getElementById('b64-total-cases')!.innerHTML = cases.base64.length;
    // @ts-ignore ts can't prove to itself that the element exists
    document.getElementById('b64-go')!.onclick            = b64_tests;

    // @ts-ignore ts can't prove to itself that the element exists
    document.getElementById('b58-total-cases')!.innerHTML = cases.base58.length;
    // @ts-ignore ts can't prove to itself that the element exists
    document.getElementById('b58-go')!.onclick            = b58_tests;
}
main();
