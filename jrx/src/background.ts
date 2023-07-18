/**
 * JR Controller
 *
 * Note that this is a *thread*.  We put our business and storage logic within
 * this thread.  The reason we do that is we don't want to deal with the
 * problem of conflicting concurrent writes..
 *
 * @module
 */

export {
};

import * as awcp from './jex_include/local-awcp-0.2.1/dist/awcp.js';

/**
 * @example
 * ```json
 * {
 *     // EventData_A2w
 *     "type": "to_waellet",
 *     "data": {
 *         // RpcCall
 *         "jsonrpc": "2.0",
 *         "id": "ske-connect-1",
 *         "method": "connection.open",
 *         "params": {
 *             // Params_A2W_connection_open
 *             "name": "sidekick examples",
 *             "version": 1
 *         }
 *     }
 * }
 * ```
 */
                                                 // method  params
type a2w_calldata = awcp.EventData_A2W<awcp.RpcCall<string, any>>;


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



/**
 * Handle messages from the content script
 *
 * Note to self: `sendResponse` is fake. Doesn't work. Instead just return the
 * response
 */
async function
bg_a2w_handler
    (msg: a2w_calldata, sender: any, sendResponse: any)
    : Promise<void>
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
                           address      : {current   : {"boobs": {}},
                                           connected : {}}});
    }
}



/**
 * main function for background thread
 */
async function
jr_bg_main
    ()
    : Promise<void>
{
    console.log('hi');

    // handle messages from content script
    browser.runtime.onMessage.addListener(bg_a2w_handler);

}


jr_bg_main();
