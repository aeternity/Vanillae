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
 * # Naming Conventions
 *
 * -   `b_...`   -> background script API
 * -   `bi_...`  -> background internal
 * -   `bis_...` -> background internal storage
 *     storage is in json types so we need a separate type layer
 *
 * -   `p_...`   -> popup script API
 *
 * @module
 */

import * as awcp from './jex_include/local-awcp-0.2.2/dist/awcp.js';


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

    // get state
    let state: bi_state = await bi_get_state();
    console.log(state);

    // handle messages from content or popup scripts
    browser.runtime.onMessage.addListener(bi_runtime_msg_handler);
}



//=============================================================================
//=============================================================================
// INTERNALS: TYPES
//=============================================================================
//=============================================================================

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
    //console.log('msg', msg);
    //console.log('sender', sender);
    //console.log('sendResponse', sendResponse);

    let msg_method : string          = msg.data.method;
    let msg_id     : string | number = msg.data.id;

    function w2a_ok(result_for_content_script: any) {
        return bi_mk_w2a_msg_ok(msg_method, msg_id, result_for_content_script);
    }

    switch (msg.data.method) {
        case "connection.open":
            return w2a_ok({id        : "jr",
                           name      : "JR",
                           networkId : "ae_uat",
                           origin    : browser.runtime.getURL('/'),
                           type      : "extension"});

        // TODO: factor this out into a function
        case "address.subscribe":
            // get the state
            let i_state : bi_state = await bi_get_state();

            // get the keypairs
            // for now only getting the first one
            let address_bytes : Uint8Array = i_state.keypairs[0].publicKey;

            // convert it to a string
            let address_ak_str : string    = vdk_aeser.pubkey2ak_str(address_bytes);

            return w2a_ok({subscription : ["connected"],
                           address      : {current   : {address_ak_str: {}},
                                           connected : {}}});
    }
}



/**
 * Wrap up bs for w2a message
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
 * if there is no state make an initial state and store it
 *
 * @internal
 */
async function
bi_get_state
    ()
    : Promise<bi_state>
{
    console.log('jr bg bi_get_state');
    // this may or may not have the keyword "jr_state"
    let gotten_state : {jr_state?: bi_state} = await browser.storage.local.get();

    console.log('jr bg gotten_state', gotten_state);

    // problem: browser storage is JSON basically
    // so we need to convert our state to and from json

    // if there is such a state, get it
    // @ts-ignore ts doesn't understand querying if a key exists
    if (!!(gotten_state.jr_state)) {
        // @ts-ignore ts doesn't understand that we have proved that the key exists
        return bi_s2i(gotten_state);
    }
    // otherwise return default state
    else {
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
 * 2. This function is **NOT** deterministic. It randomly generates a keypair
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
