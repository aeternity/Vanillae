export {
    decoded_data,
    decode_result,
    decode
}

type decoded_data
    = Uint8Array
    | Array<decoded_data>;

type decode_result
    = {decoded_data : decoded_data,
       remainder    : Uint8Array};

function
decode(bytes: Uint8Array): decode_result {
    // check the first byte
    let first_byte: number      = bytes[0];
    let rest       : Uint8Array = bytes.slice(1);
    // if the first byte is between 0 and 127, that is the data
    if
    (first_byte <= 127) {
        return dr(new Uint8Array([first_byte]), rest);
    }
    // if the first byte is between 128 and 183 = 128 + 55, it is a bytestring
    // and the length is Byte - 128
    else if
    (first_byte <= 183) {
        let payload_byte_length : number     = first_byte - 128;
        let payload             : Uint8Array = rest.slice(0, payload_byte_length);
        let rest2               : Uint8Array = rest.slice(payload_byte_length);
        return dr(payload, rest2);
    }
    // if the first byte is between 184 = 183 + 1 and 191 = 183 + 8, it is a
    // bytestring. the byte length of bytestring is FirstByte - 183. Then pull
    // out the actual data
    else if
    (first_byte <= 191) {
        let byte_length_of_byte_length : number     = first_byte - 183;
        let bytes_of_byte_length       : Uint8Array = rest.slice(0, byte_length_of_byte_length);
        let byte_length                : number     = bytes_to_number(bytes_of_byte_length);
        let bytes                      : Uint8Array = rest.slice(byte_length_of_byte_length,
                                                                 byte_length + byte_length_of_byte_length);
        let rest2                      : Uint8Array = rest.slice(byte_length + byte_length_of_byte_length);
        return dr(bytes, rest2);
    }
    // If the first byte is between 192 and 247 = 192 + 55, it is a list. The
    // byte length of the list-payload is FirstByte - 192. Then the list
    // payload, which needs to be decoded on its own.
    else if
    (first_byte <= 247) {
        let byte_length_of_list : number              = first_byte - 192;
        let list_payload        : Uint8Array          = rest.slice(0, byte_length_of_list);
        let list                : Array<decoded_data> = decode_list(list_payload);
        let rest2               : Uint8Array          = rest.slice(byte_length_of_list);
        return dr(list, rest2);
    }
    // If the first byte is between 248 = 247 + 1 and 255 = 247 + 8, it is a
    // list.  The byte length of the byte length of the list-payload is
    // FirstByte - 247.  Then the byte length of the list. Then the list
    // payload, which needs to be decoded on its own.
    else {
        let byte_length_of_byte_length : number              = first_byte - 247;
        let bytes_of_byte_length       : Uint8Array          = rest.slice(0, byte_length_of_byte_length);
        let byte_length                : number              = bytes_to_number(bytes_of_byte_length);
        let list_bytes                 : Uint8Array          = rest.slice(byte_length_of_byte_length,
                                                                          byte_length + byte_length_of_byte_length);
        let list                       : Array<decoded_data> = decode_list(list_bytes);
        let rest2                      : Uint8Array          = rest.slice(byte_length + byte_length_of_byte_length);
        return dr(list, rest2);
    }
}

function decode_list(bytes: Uint8Array): Array<decoded_data> {
    let arr : Array<decoded_data> = [];
    while (bytes.length > 0) {
        // grab an item off the bytes
        let {decoded_data, remainder} = decode(bytes);
        // push it
        arr.push(decoded_data);
        // update bytes
        bytes = remainder;
    }
    return arr;
}

// convert bytestring to number
function bytes_to_number(bytes: Uint8Array) {
    let n : number = 0;
    for (let b of bytes) {
        n <<= 8;
        n  += b;
    }
    return n;
}

function dr(x : decoded_data, y : Uint8Array) {
    return {decoded_data: x, remainder: y};
}
