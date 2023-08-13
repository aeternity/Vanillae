/**
 * JR Controller "Library".
 *
 * Conceptually this is the thread corresponding to the application controller.
 * The background script calls `b_main()` and does nothing else. The popup
 * script calls the various `p_...` functions, which black-box away the IPC
 * nonsense.  Really, conceptually, the popup script is making a call to the
 * background script, and the code idiom merely reflects that conceptual
 * reality.
 *
 * In a perfect world, the content script would also act like this. However,
 *
 * 1. The content script is already a finished product. All it needs to do is
 *    relay messages back and forth between this thread and the page script,
 *    and it does that just fine.
 * 2. The content script isn't an ES6 module, and so "imports" are a giant pain
 *    in the ass. So rewriting it to fit this "call the background script"
 *    idiom is just not worth it.  It already works.  The meaningful IPC is
 *    between the page script and a generic wallet, and that is already defined
 *    in AWCP.
 *
 * # Naming/Notation Conventions
 *
 * ```
 * - b_...   -> background script API
 * - bi_...  -> background internal
 * - bis_... -> background internal storage
 * - p_...   -> popup script API
 * - !...    -> bookmark for regex searches to quickly find a section
 *              bang = bookmark
 * ```
 *
 *
 * Re storage: the browser's storage layer for extensions basically stores
 * the data in JSON, so we need a separate type layer to make sure the
 * conversion between the program's internal state and what the browser
 * stores doesn't get fucked up.  Despite the fact that JSON is magical fairy
 * dust that seamlessly encodes/decodes losslessly to and from any type, I
 * still am much more comfortable writing it out myself.
 *
 *
 * FIXME: check bytes or sth for keypair storage to guard against json fuckery
 * FIXME: versioned storage
 * FIXME: move some of the AWCP data making functions to the AWCP lib
 * FIXME: the address_subscribe data scraping nonsense needs to be factored out
 * FIXME: quiet mode (undetectable by default)
 * FIXME: ask user if he wants to connect/address/etc
 * FIXME: ask user if he wants to sign messages
 *
 * @module
 */


import * as awcp        from './jex_include/local-awcp-0.2.2/dist/awcp.js';
import * as vdk_aecrypt from './jex_include/local-vdk_aecrypt-0.1.0/dist/vdk_aecrypt.js';
import * as vdk_aeser   from './jex_include/local-vdk_aeser-0.1.0/dist/vdk_aeser.js';
import * as vdk_binary  from './jex_include/local-vdk_binary-0.1.0/dist/vdk_binary.js';


export type {
    // global types
    // popup script call/return data
};


export {
    // popup script api
    // background script api
    b_main
};



//=============================================================================
//=============================================================================
// API: GLOBAL TYPES
//=============================================================================
//=============================================================================


//=============================================================================
//=============================================================================
// API: POPUP TYPES
//=============================================================================
//=============================================================================


//=============================================================================
//=============================================================================
// API: POPUP SCRIPT
//=============================================================================
//=============================================================================


//=============================================================================
//=============================================================================
// API: BACKGROUND SCRIPT
//=============================================================================
//=============================================================================

/**
 * main function for background thread
 */
async function
b_main
    ()
    : Promise<void>
{
    console.log('b_main');

    // handle messages from content or popup scripts
    browser.runtime.onMessage.addListener(bi_runtime_msg_handler);
}



//=============================================================================
//=============================================================================
// INTERNALS: TYPES
//=============================================================================
//=============================================================================
// !bi_types !bi_state !bi-types !bi-state

/**
 * @example
 * ```ts
 * // EventData_A2w
 * {type : "to_waellet",
 *         // RpcCall
 *  data : {jsonrpc : "2.0",
 *          id      : "ske-connect-1",
 *          method  : "connection.open",
 *                    // Params_A2W_connection_open
 *          params  : {name    : "sidekick examples",
 *                     version : 1}}}
 * ```
 *
 * @internal
 */
type bi_a2w_calldata
    = awcp.EventData_A2W<awcp.RpcCall<string, any>>;
                                   // method  params



/**
 * Return data for queries from a page script
 *
 * @internal
 */
type bi_w2a_return_data
    = awcp.EventData_W2A<awcp.RpcResp<string, any>>;



/**
 * Type copied from NaCL
 *
 * @internal
 */
type bi_nacl_keypair
    = {secretKey : Uint8Array,
       publicKey : Uint8Array};



/**
 * JR state used internally
 *
 * @internal
 */
type bi_state
    = {keypairs: Array<bi_nacl_keypair>};



/**
 * JR storage state
 *
 * Storage is in JSON so we have to have a different type for storage and for
 * what's actually used during ordinary data processing.
 *
 * @internal
 */
type bis_state
    = {jr_state: {keypairs: Array<bis_nacl_keypair>}};



/**
 * How keypairs are stored
 *
 * @internal
 */
type bis_nacl_keypair
    = {secretKey : Array<number>,
       publicKey : Array<number>};



//=============================================================================
//=============================================================================
// INTERNALS: FUNCTIONS
//=============================================================================
//=============================================================================


/**
 * Handle messages from either content or popup script
 *
 * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
 * response
 *
 * @internal
 */
async function
bi_runtime_msg_handler
    (msg          : {frum: 'content' | 'popup',
                     data: any},
     sender       : any,
     sendResponse : any)
    : Promise<bi_w2a_return_data | bi_popup_return_data>
{
    //console.log('msg', msg);
    //console.log('sender', sender);
    //console.log('sendResponse', sendResponse);

    switch (msg.frum) {
        // messages from the content script (i.e. from page scripts)
        case 'content':
            return await bi_msg_handler_content(msg.data as bi_a2w_calldata, sender, sendResponse);
        // messages from the popup window
        case 'popup':
            return await bi_msg_handler_popup(msg.data as bi_popup_calldata, sender, sendResponse);
    }
}



//-----------------------------------------------------------------------------
// INTERNALS: CONTENT SCRIPT MESSAGE HANDLING
//-----------------------------------------------------------------------------

/**
 * Handle messages from the content script
 *
 * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
 * response.
 *
 * Literally the way it works is that
 *
 * - IF this is a non-async function (i.e. does NOT return a `Promise`), THEN
 *   `sendResponse` is fake and the runtime sends back the return value of the
 *   function. This is our case.
 * - ELSE `sendResponse` works. Presumably this is for backward compabitility.
 *
 * @internal
 */
async function
bi_msg_handler_content
    (msg          : bi_a2w_calldata,
     sender       : any,
     sendResponse : any)
    : Promise<bi_w2a_return_data>
{
    console.log('bi_msg_handler_content', {msg: msg});
    //console.log('msg', msg);
    //console.log('sender', sender);
    //console.log('sendResponse', sendResponse);

    let msg_method : string          = msg.data.method;
    let msg_id     : string | number = msg.data.id;

    // get the state
    let i_state : bi_state = await bi_get_state();


    // function to encode the ok message
    // depends on parameters above so makes sense to have it be a lambda
    function w2a_ok(result_for_content_script: any) {
        return bi_mk_w2a_msg_ok(msg_method,
                                msg_id,
                                result_for_content_script);
    }

    // function to encode the error message
    // depends on parameters above so makes sense to have it be a lambda
    function w2a_err(code: number, message: string) {
        return bi_mk_w2a_msg_err(msg_method,
                                 msg_id,
                                 code,
                                 message);
    }


    console.log('jr bg content message handler method:', msg.data.method);
    switch (msg.data.method) {
        // right now just give connect info back
        case "connection.open":
            return w2a_ok({id        : "jr",
                           name      : "JR",
                           networkId : "ae_uat",
                           origin    : browser.runtime.getURL('/'),
                           type      : "extension"});

        // right now just give address back
        case "address.subscribe":
            console.log('jr bg content message handler address.subscribe');
            // get the keypairs
            // for now only getting the first one
            let address_bytes : Uint8Array = i_state.keypairs[0].publicKey;
            // convert it to a string
            let address_ak_str : string    = await vdk_aeser.pubkey2ak_str(address_bytes);
            return w2a_ok(bi_address_reply(address_ak_str, []));

        // right now just sign message
        case "message.sign":
            console.log('jr bg content message handler message sign');
            let secret_key        : Uint8Array = i_state.keypairs[0].secretKey;
            let msg_str           : string     = msg.data.params.message;
            // use nacl detached signatures
            // https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L141-L143
            // https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L160-L167
            let hashed_salted_msg : Uint8Array = vdk_aecrypt.hash_and_salt_msg(msg_str);
            let signature         : Uint8Array = nacl.sign.detached(hashed_salted_msg, secret_key);
            let signature_str     : string     = vdk_binary.bytes_to_hex_str(signature);
            return w2a_ok({signature: signature_str});


        // default is NYI
        default:
            return w2a_err(awcp.ERROR_CODE_RpcMethodNotFoundError, 'not yet implemented');
    }
}


/**
 * Make the dumb address_subscribe thing
 *
 * @internal
 */
function
bi_address_reply
    (current_pubkey_str : string,
     other_pubkey_strs  : Array<string>)
    : awcp.Result_W2A_address_subscribe
{
    // From the AWCP docs:
    //
    //      @example
    //      This is if the user has many keypairs. The currently selected one is under
    //      `current`.  Craig, I agree this is stupid, but that's how it works.
    //
    //      {
    //          "subscription": [
    //              "connected"
    //          ],
    //          "address": {
    //              "current": {
    //                  "ak_25C3xaAGQddyKAnaLLMjAhX24xMktH2NNZxY3fMaZQLMGED2Nf": {}
    //              },
    //              "connected": {
    //                  "ak_BMtPGuqDhWLnMVL4t6VFfS32y2hd8TSYwiYa2Z3VdmGzgNtJP": {},
    //                  "ak_25BqQuiVCasiqTkXHEffq7XCsuYEtgjNeZFeVFbuRtJkfC9NyX": {},
    //                  "ak_4p6gGoCcwQzLXd88KhdjRWYgd4MfTsaCeD8f99pzZhJ6vzYYV": {}
    //              }
    //          }
    //      }

    // so we make the "object" in the "current" field
    // this is so dumb but
    let current_obj = {};
    // @ts-ignore shut up tsc i know this is stupid it's not my fault
    current_obj[current_pubkey_str] = {};

    // make the object in the "connected" field
    let connected_obj = {};
    for (let this_pubkey_str of other_pubkey_strs) {
        // @ts-ignore shut up tsc i know this is stupid it's not my fault
        connected_obj[this_pubkey_str] = {};
    }

    return {subscription : ['connected'],
            address      : {current   : current_obj,
                            connected : connected_obj}};
}



/**
 * SUCCESS CASE: Wrap up bs for w2a message
 *
 * This really should go into the AWCP library but I'm lazy
 *
 * @internal
 */
function
bi_mk_w2a_msg_ok
    <result_t extends any>
    (method : string,
     id     : string | number,
     result : result_t)
    : awcp.EventData_W2A<awcp.RpcResp_ok<string, result_t>>
{
    return {type : "to_aepp",
            data : {jsonrpc : "2.0",
                    method  : method,
                    id      : id,
                    result  : result}};
}



/**
 * ERROR CASE: Wrap up bs for w2a message
 *
 * This really should go into the AWCP library but I'm lazy
 *
 * @internal
 */
function
bi_mk_w2a_msg_err
    (method  : string,
     id      : string | number,
     code    : number,
     message : string)
    : awcp.EventData_W2A<awcp.RpcResp_error<string>>
{
    return {type : "to_aepp",
            data : {jsonrpc : "2.0",
                    method  : method,
                    id      : id,
                    error   : {code    : code,
                               message : message}}};
}



//----------------------------------------------------------------------------
// INTERNALS: POPUP WINDOW MESSAGE HANDLING
//----------------------------------------------------------------------------


type bi_popup_calldata
    = 'init';


type bi_popup_return_data
    = 'die';


/**
 * Handle messages from the popup
 *
 * @internal
 */
async function
bi_msg_handler_popup
    (msg          : bi_popup_calldata,
     sender       : any,
     sendResponse : any)
    : Promise<bi_popup_return_data>
{
    console.log('jr bg_msg_handler_popup msg', msg);
    return 'die';
}



//=============================================================================
//=============================================================================
// INTERNALS: STORAGE API
//=============================================================================
//=============================================================================


/**
 * Fetch the state from local storage
 *
 * If there is no state make an initial state and store it
 *
 * @internal
 */
async function
bi_get_state
    ()
    : Promise<bi_state>
{
    console.log('jr bg bi_get_state');

    // so
    // - if extension has NEVER committed state (i.e. this is the first
    //   invocation), there will be NO key "jr_state"
    // - if we have committed state, that key will exist
    //
    // right now our game is branching on whether or not there is existing
    // state in the browser's storage
    //
    // - if there is state, we simply fetch it from storage, convert it to a
    //   type suitable for use in code (e.g. keypairs are byte arrays rather
    //   than dumb JSON number arrays)
    // - if there is no state, we create a default initial state, commit it,
    //   and hand it back to the calling code

    // this may or may not have the keyword "jr_state"
    let gotten_state : ({} | bis_state) = await browser.storage.local.get();

    console.log('jr bg gotten_state', gotten_state);

    // problem: browser storage is JSON basically
    // so we need to convert our state to and from json


    // if there is such a state, get it
    // @ts-ignore ts doesn't understand querying if a key exists
    if (!!(gotten_state.jr_state)) {
        console.log('foo');
        // TS doesn't know we've proven the key exists and so therefore this is
        // of type bis_state
        return bi_s2i(gotten_state as bis_state);
    }
    // otherwise return default state
    else {
        console.log('bar');
        // set the state
        let default_i_state : bi_state  = bi_state_default();
        await bi_set_state(default_i_state);
        // return it
        return default_i_state;
    }
}



/**
 * Set the state. Blocks until state is set, exception is thrown if there is an
 * exception.
 *
 * @internal
 */
async function
bi_set_state
    (i_state : bi_state)
    : Promise<void>
{
    console.log('jr bg bi_set_state internal state:', i_state);

    // convert to storage state
    let s_state : bis_state = bi_i2s(i_state);

    console.log('jr bg bi_set_state storage state:', s_state);

    // store
    await browser.storage.local.set(s_state);
}



//-----------------------------------------------------------------------------
// INTERNALS: INTERNAL->STORAGE TYPE COERCION
//-----------------------------------------------------------------------------

/**
 * Internal state to storage state converter
 *
 * @internal
 */
function
bi_i2s
    (internal_state : bi_state)
    : bis_state
{
    // convert keypairs
    let i_keypairs : Array<bi_nacl_keypair>  = internal_state.keypairs;
    let s_keypairs : Array<bis_nacl_keypair> = i_keypairs.map(bi_i2s_keypair);

    return {jr_state : {keypairs: s_keypairs}};
}



/**
 * JSONify a keypair
 *
 * basically turn each array into numbers
 *
 * @internal
 */
function
bi_i2s_keypair
    (i_keypair : bi_nacl_keypair)
    : bis_nacl_keypair
{
    let i_secretKey : Uint8Array    = i_keypair.secretKey;
    let i_publicKey : Uint8Array    = i_keypair.publicKey;
    let s_secretKey : Array<number> = bi_i2s_bytes2nums(i_secretKey);
    let s_publicKey : Array<number> = bi_i2s_bytes2nums(i_publicKey);
    return {secretKey : s_secretKey,
            publicKey : s_publicKey};
}



/**
 * Convert a byte array to an array of numbers
 *
 * @internal
 */
function
bi_i2s_bytes2nums
    (bytes: Uint8Array)
    : Array<number>
{
    let arr : Array<number> = [];
    for (let this_byte of bytes) {
        arr.push(this_byte);
    }
    return arr;
}



/**
 * Default state for JR
 *
 * Note
 * 1. This returns the *internal* state that lives in RAM, **NOT** what is
 *    stored in browser storage
 * 2. This function is **NOT** deterministic. It randomly generates a keypair.
 *
 * @internal
 */
function
bi_state_default
    ()
    : bi_state
{
    console.log('jr_state_default');
    // if no key, generate one
    // @ts-ignore ts doesn't like that nacl is dumb. i don't like it either
    let init_keypair  : bi_nacl_keypair = nacl.sign.keyPair() as bi_nacl_keypair;
    let default_state : bi_state        = { keypairs: [init_keypair] };
    console.log('jr_state_default default_state', default_state);
    return default_state;
}



//-----------------------------------------------------------------------------
// INTERNALS: STORAGE->INTERNAL TYPE COERCION
//-----------------------------------------------------------------------------

/**
 * Storage -> internal state converter
 *
 * @internal
 */
function
bi_s2i
    (s_state : bis_state)
    : bi_state
{
    // for now bis_state is just keypairs
    let s_keypairs : Array<bis_nacl_keypair> = s_state.jr_state.keypairs;

    // convert numbers to byte array
    function nums2bytes(nums: Array<number>): Uint8Array {
        return new Uint8Array(nums);
    }

    // convert storage keypair to normal keypair
    function keypair_s2i(s_keypair: bis_nacl_keypair): bi_nacl_keypair {
        let s_secretKey : Array<number> = s_keypair.secretKey;
        let s_publicKey : Array<number> = s_keypair.publicKey;
        let i_secretKey : Uint8Array    = nums2bytes(s_secretKey);
        let i_publicKey : Uint8Array    = nums2bytes(s_publicKey);
        return {secretKey : i_secretKey,
                publicKey : i_publicKey};
    }

    // convert each keypair
    let i_keypairs : Array<bi_nacl_keypair> = s_keypairs.map(keypair_s2i);

    return {keypairs: i_keypairs};
}
