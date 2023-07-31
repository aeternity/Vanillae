/**
 * JR Controller
 *
 * Note that this is a *thread*.  We put our business and storage logic within
 * this thread.  The reason we do that is we don't want to deal with the
 * problem of conflicting concurrent writes..
 *
 * # Naming Conventions
 *
 * - `b_...` -> internal to background thread
 * - `c_...` -> meant to be called/used from content script
 * - `p_...` -> meant to be called/used from popup script
 *
 * @module
 */

//import * as awcp from './jex_include/local-awcp-0.2.1/dist/awcp.js';


export type {
    // content script call/return data
    // popup script call/return data
};


export {
    // bg calling code
    b_main
    // content script api
    // popup script api
};

//
//
////=============================================================================
//// TYPES
////=============================================================================
//
//
///**
// * @example
// * ```json
// * {
// *     // EventData_A2w
// *     "type": "to_waellet",
// *     "data": {
// *         // RpcCall
// *         "jsonrpc": "2.0",
// *         "id": "ske-connect-1",
// *         "method": "connection.open",
// *         "params": {
// *             // Params_A2W_connection_open
// *             "name": "sidekick examples",
// *             "version": 1
// *         }
// *     }
// * }
// * ```
// *
// * @internal
// */
//type a2w_calldata
//    = awcp.EventData_A2W<awcp.RpcCall<string, any>>;
//                                   // method  params
//
//
//
///**
// * Return data
// */
//type w2a_return_data
//    = awcp.EventData_W2A<awcp.RpcResp<string, any>>;
//
//
//
///**
// * Type from NaCL
// */
//type nacl_keypair
//    = {secretKey : Uint8Array,
//       publicKey : Uint8Array};
//
//
//
///**
// * JR state
// */
//type jr_state = {keypairs: Array<nacl_keypair>};
//
//
/**
 * main function for background thread
 */
async function
b_main
    ()
    : Promise<void>
{
    console.log('b_main');

    //// get state
    //let state: jr_state = await get_state();
    //console.log(state);

    //// handle messages from content or popup scripts
    //browser.runtime.onMessage.addListener(bg_runtime_msg_handler);

}
//
//
//
//
///**
// * fetch the state from local storage
// *
// * if there is no state make an initial state and store it
// */
//async function
//b_get_state
//    ()
//    : Promise<jr_state>
//{
//    console.log('jr bg get_state');
//    // this may or may not have the keyword "jr_state"
//    let gotten_state : object = await browser.storage.local.get();
//
//    console.log('jr bg gotten_state', gotten_state);
//
//    // if there is such a state, get it
//    // @ts-ignore ts doesn't understand querying if a key exists
//    if (!!(gotten_state.jr_state)) {
//        // @ts-ignore ts doesn't understand that we have proved that the key exists
//        return gotten_state.jr_state;
//    }
//    // otherwise return default state
//    else {
//        return jr_state_default();
//    }
//}
//
//
//
///**
// * default state for JR
// */
//function
//jr_state_default
//    ()
//    : jr_state
//{
//    console.log('jr_state_default');
//    // if no key, generate one
//    // @ts-ignore ts doesn't like that nacl is dumb. i don't like it either
//    let init_keypair : nacl_keypair = nacl.sign.keyPair() as nacl_keypair;
//    let default_state = { keypairs: [init_keypair] };
//    console.log('jr_state_default default_state', default_state);
//    return default_state;
//}
//
//
//
///**
// * wrap up bs for w2a message
// */
//function
//mk_w2a_msg_ok
//    <result_t extends any>
//    (method : string,
//     id     : string | number,
//     result : result_t)
//    : awcp.EventData_W2A<awcp.RpcResp_ok<string, result_t>>
//{
//    return {type : "to_aepp",
//            data : {jsonrpc : "2.0",
//                    method  : method,
//                    id      : id,
//                    result  : result}};
//}
//
//
//
///**
// * Handle messages from either content or popup script
// *
// * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
// * response
// */
//async function
//bg_runtime_msg_handler
//    (msg          : {frum: 'content' | 'popup',
//                     data: any},
//     sender       : any,
//     sendResponse : any)
//    : Promise<w2a_return_data | popup_return_data>
//{
//    //console.log('msg', msg);
//    //console.log('sender', sender);
//    //console.log('sendResponse', sendResponse);
//
//    switch (msg.frum) {
//        case 'content':
//            return await bg_a2w_handler_content(msg.data as a2w_calldata, sender, sendResponse);
//        case 'popup':
//            return await bg_msg_handler_popup(msg.data as popup_calldata, sender, sendResponse);
//    }
//}
//
//
///**
// * Handle messages from the content script
// *
// * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
// * response
// */
//async function
//bg_a2w_handler_content
//    (msg          : a2w_calldata,
//     sender       : any,
//     sendResponse : any)
//    : Promise<w2a_return_data>
//{
//    //console.log('msg', msg);
//    //console.log('sender', sender);
//    //console.log('sendResponse', sendResponse);
//
//    let msg_method : string          = msg.data.method;
//    let msg_id     : string | number = msg.data.id;
//
//    function w2a_ok(result_for_content_script: any) {
//        return mk_w2a_msg_ok(msg_method, msg_id, result_for_content_script);
//    }
//
//    switch (msg.data.method) {
//        case "connection.open":
//            return w2a_ok({id        : "jr",
//                           name      : "JR",
//                           networkId : "ae_uat",
//                           origin    : "foobar",
//                           type      : "extension"});
//        case "address.subscribe":
//            return w2a_ok({subscription : ["connected"],
//                           address      : {current   : {"boobs": {}},
//                                           connected : {}}});
//    }
//}
//
//
//
//
//type popup_calldata
//    = 'init';
//
//
//type popup_return_data
//    = 'die';
//
//
///**
// * Handle messages from the popup
// */
//async function
//bg_msg_handler_popup
//    (msg          : popup_calldata,
//     sender       : any,
//     sendResponse : any)
//    : Promise<popup_return_data>
//{
//    console.log('jr bg_msg_handler_popup msg', msg);
//    return 'die';
//}
