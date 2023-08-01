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
 * - `b_...` -> background script API
 * - `_...` -> internal
 * - `p_...` -> popup script API
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
// API: GLOBAL TYPES
//=============================================================================


//=============================================================================
// API: POPUP TYPES
//=============================================================================


//=============================================================================
// API: POPUP SCRIPT
//=============================================================================


//=============================================================================
// API: BACKGROUND SCRIPT
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
    let state: _state = await _get_state();
    console.log(state);

    // handle messages from content or popup scripts
    browser.runtime.onMessage.addListener(_runtime_msg_handler);
}



//=============================================================================
// INTERNALS: TYPES
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
type _a2w_calldata
    = awcp.EventData_A2W<awcp.RpcCall<string, any>>;
                                   // method  params



/**
 * Return data
 */
type _w2a_return_data
    = awcp.EventData_W2A<awcp.RpcResp<string, any>>;



/**
 * Type from NaCL
 */
type _nacl_keypair
    = {secretKey : Uint8Array,
       publicKey : Uint8Array};



/**
 * JR state
 */
type _state = {keypairs: Array<_nacl_keypair>};




//=============================================================================
// INTERNALS: FUNCTIONS
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
_runtime_msg_handler
    (msg          : {frum: 'content' | 'popup',
                     data: any},
     sender       : any,
     sendResponse : any)
    : Promise<_w2a_return_data | _popup_return_data>
{
    //console.log('msg', msg);
    //console.log('sender', sender);
    //console.log('sendResponse', sendResponse);

    switch (msg.frum) {
        case 'content':
            return await _msg_handler_content(msg.data as _a2w_calldata, sender, sendResponse);
        case 'popup':
            return await _msg_handler_popup(msg.data as _popup_calldata, sender, sendResponse);
    }
}



/**
 * Handle messages from the content script
 *
 * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
 * response
 */
async function
_msg_handler_content
    (msg          : _a2w_calldata,
     sender       : any,
     sendResponse : any)
    : Promise<_w2a_return_data>
{
    //console.log('msg', msg);
    //console.log('sender', sender);
    //console.log('sendResponse', sendResponse);

    let msg_method : string          = msg.data.method;
    let msg_id     : string | number = msg.data.id;

    function w2a_ok(result_for_content_script: any) {
        return mk_w2a_msg_ok(msg_method, msg_id, result_for_content_script);
    }

    switch (msg.data.method) {
        case "connection.open":
            return w2a_ok({id        : "jr",
                           name      : "JR",
                           networkId : "ae_uat",
                           origin    : "foobar",
                           type      : "extension"});
        case "address.subscribe":
            return w2a_ok({subscription : ["connected"],
                           address      : {current   : {"bobs": {}},
                                           connected : {}}});
    }
}








/**
 * fetch the state from local storage
 *
 * if there is no state make an initial state and store it
 */
async function
_get_state
    ()
    : Promise<_state>
{
    console.log('jr bg _get_state');
    // this may or may not have the keyword "jr_state"
    let gotten_state : object = await browser.storage.local.get();

    console.log('jr bg gotten_state', gotten_state);

    // if there is such a state, get it
    // @ts-ignore ts doesn't understand querying if a key exists
    if (!!(gotten_state.jr_state)) {
        // @ts-ignore ts doesn't understand that we have proved that the key exists
        return gotten_state.jr_state;
    }
    // otherwise return default state
    else {
        return _state_default();
    }
}



/**
 * default state for JR
 */
function
_state_default
    ()
    : _state
{
    console.log('jr_state_default');
    // if no key, generate one
    // @ts-ignore ts doesn't like that nacl is dumb. i don't like it either
    let init_keypair : nacl_keypair = nacl.sign.keyPair() as nacl_keypair;
    let default_state = { keypairs: [init_keypair] };
    console.log('jr_state_default default_state', default_state);
    return default_state;
}



/**
 * wrap up bs for w2a message
 */
function
mk_w2a_msg_ok
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







type _popup_calldata
    = 'init';


type _popup_return_data
    = 'die';


/**
 * Handle messages from the popup
 */
async function
_msg_handler_popup
    (msg          : _popup_calldata,
     sender       : any,
     sendResponse : any)
    : Promise<_popup_return_data>
{
    console.log('jr bg_msg_handler_popup msg', msg);
    return 'die';
}
