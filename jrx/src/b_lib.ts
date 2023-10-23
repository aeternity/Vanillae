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
 * stores doesn't get fornicated up.  Despite the fact that JSON is magical
 * fairy dust that seamlessly encodes/decodes losslessly to and from any type,
 * I still am much more comfortable writing it out myself.
 *
 *
 * FIXME: check bytes or sth for keypair storage to guard against json fornicateery
 * FIXME: versioned storage
 * FIXME: move some of the AWCP data making functions to the AWCP lib
 * FIXME: the address_subscribe data scraping nonsense needs to be factored out
 * FIXME: quiet mode (undetectable by default)
 * FIXME: ask user if he wants to connect/address/etc
 * FIXME: ask user if he wants to sign messages
 *
 * @module
 */


import * as awcp        from './jex_include/local-awcp-0.2.3/dist/awcp.js';
import * as vdk_aecrypt from './jex_include/local-vdk_aecrypt-0.1.2/dist/vdk_aecrypt.js';
import * as vdk_aeser   from './jex_include/local-vdk_aeser-0.1.1/dist/vdk_aeser.js';
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

// INTERNAL CONSTANTS ALL IN ONE PLACE
let GB_ITER_MS = 5;             // [MS / ITER] 5 ms every 1 iteration
let GB_ITERS   = 1;
let GB_SEC     = 200*GB_ITERS;  // [ITER]; 1iter / 5ms = 200iter / sec
let GB_MIN     = 60*GB_SEC;     // [ITER]

let MSG_SIGN_TIMEOUT = 30*GB_MIN;
let TX_SIGN_TIMEOUT  = 30*GB_MIN;


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

    let public_key        : Uint8Array = i_state.keypairs[0].publicKey;
    let secret_key        : Uint8Array = i_state.keypairs[0].secretKey;

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
            // convert it to a string
            let address_ak_str : string    = await vdk_aeser.pubkey2ak_str(public_key);
            console.log('public key:', address_ak_str);
            return w2a_ok(bi_address_reply(address_ak_str, []));

        // right now just sign message
        case "message.sign":
            console.log('jr bg content message handler message sign');
            let msg_str : string = msg.data.params.message;
            // TODONE: need to break here on whether the msg signature was good
            // (signed) or bad (not)
            let resultt = await msg_sign(msg_str, secret_key);
            if (resultt.result === 'good') {
                return w2a_ok({signature: resultt.signature});
            }
            else {
                return w2a_err(awcp.ERROR_CODE_RpcRejectedByUserError, "User either rejected message signature request or did not confirm within 30 minutes of popup window shown.");
            }

        // right now just sign tx
        case "transaction.sign":
            console.log('jr bg content message handler transaction sign');
            let tx_str : string = msg.data.params.tx;
            console.log('transaction: ', tx_str);
            let result = await tx_sign(tx_str, secret_key)
            if
            ('good' === result.result) {
                return w2a_ok({signedTransaction: result.signedTransaction});
            }
            else {
                return w2a_err(awcp.ERROR_CODE_RpcRejectedByUserError, "your not pretty enough");
            }
            //if (result.result === 'good') {
            //    return w2a_ok({signedTransaction: result.signedTransaction});
            //}
            //else {
            //    return w2a_err(awcp.ERROR_CODE_RpcRejectedByUserError, "you will never be pretty enough");
            //}

        // default is NYI
        default:
            return w2a_err(awcp.ERROR_CODE_RpcMethodNotFoundError, 'not yet implemented');
    }
}


type gb = 'good' | 'bad';


/**
 * Fornicating js block scope rules
 *
 * NOOOOOOOO YOU CAN'T DECLARE TWO DIFFERENT VARIABLES WITH THE SAME NAME IN
 * TWO DIFFERENT CASES EVEN THOUGH THEY'RE MUTUALLY EXCLUSIVE
 *
 * wojak.jpg
 *
 * Good result = user said yes
 * Bad result = user said no
 *
 * @internal
 */
async function
msg_sign
    (msg_str    : string,
     secret_key : Uint8Array)
    : Promise< {result    : 'good',
                signature : string}
             | {result    : 'bad'}
             >
{
    let result : gb = await gb('Do you want to sign this message?', msg_str, MSG_SIGN_TIMEOUT);

    // if the user wants us to sign the transaction, return the signed transaction
    // confirm
    if
    (result === 'good') {
        // use nacl detached signatures
        // https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L141-L143
        // https://github.com/aeternity/aepp-sdk-js/blob/5df22dd297abebc0607710793a7234e6761570d4/src/utils/crypto.ts#L160-L167
        //console.error('hi 1');
        let hashed_salted_msg : Uint8Array = vdk_aecrypt.hash_and_salt_msg(msg_str);
        //console.error('hi 2');
        // @ts-ignore yes nacl is stupid
        let signature         : Uint8Array = nacl.sign.detached(hashed_salted_msg, secret_key);
        let signature_str     : string     = vdk_binary.bytes_to_hex_str(signature);
        return {result    : 'good',
                signature : signature_str};
    }
    // otherwise don't
    else {
        return {result : 'bad'};
    }
}



/**
 * Fornicating js block scope rules
 *
 * NOOOOOOOO YOU CAN'T DECLARE TWO DIFFERENT VARIABLES WITH THE SAME NAME IN
 * TWO DIFFERENT CASES EVEN THOUGH THEY'RE MUTUALLY EXCLUSIVE
 *
 * wojak.jpg
 *
 * @internal
 */
async function
tx_sign
    (tx_str     : string,
     secret_key : Uint8Array)
    : Promise< {result            : 'good',
                signedTransaction : string}
             | {result : 'bad'}
             >
{
    let mansplained_tx : string = await vdk_aeser.mansplain_str(tx_str);
    let result         : gb     = await gb('Do you want to sign this transaction?',
                                           mansplained_tx,
                                           TX_SIGN_TIMEOUT);

    // if we're supposed to sign the tx, then sign it
    if
    ('good' === result) {
        let tx_bytes        : Uint8Array = (await vdk_aeser.unbaseNcheck(tx_str)).bytes;
        // thank you ulf
        // https://github.com/aeternity/protocol/tree/fd179822fc70241e79cbef7636625cf344a08109/consensus#transaction-signature
        // we sign <<NetworkId, SerializedObject>>
        // SerializedObject can either be the object or the hash of the object
        // let's stick with hash for now
        // FIXME: get network id from application state
        let network_id      : Uint8Array = vdk_binary.encode_utf8('ae_uat');
        // let tx_hash_bytes   : Uint8Array = hash(tx_bytes);
        let sign_data       : Uint8Array = vdk_binary.bytes_concat(network_id, tx_bytes);
        // @ts-ignore yes nacl is stupid
        let signature       : Uint8Array = nacl.sign.detached(sign_data, secret_key);
        let signed_tx_bytes : Uint8Array = vdk_aeser.signed_tx([signature], tx_bytes);
        let signed_tx_str   : string     = await vdk_aeser.baseNcheck('tx', signed_tx_bytes);
        // debugging
        let mansplained_stx : object     = await vdk_aeser.mansplain(signed_tx_str);
        console.log('mansplained signed tx:', mansplained_stx);
        return {result            : 'good',
                signedTransaction : signed_tx_str};
    }
    // if user rejected the tx
    else {
        return {result : 'bad'};
    }
}




/**
 * This pops up a window for the luser and asks if the thing is good or bad.
 *
 * Returns good or bad
 *
 * Timeout is in "iterations"; 1 iteration is 5 milliseconds. Use `GB_SEC` and
 * `GB_MIN` to get those units.
 *
 * # Why this is stupid
 *
 * Ok super important thing: there's a very stupid idiom in here for talking to
 * the good/bad popup window. This is not because I am stupid. This is because
 * web browsers are stupid.
 *
 * We cannot just spawn a window and then start talking to it. The problem is
 * that this isn't Erlang where the process has a mailbox and we can count on
 * the messages being delivered there and then being there when the process
 * initiates a `raseev`.
 *
 * Instead the good/bad popup-window process has to spawn *and then listen for
 * messages*, and if we send a message before it's listening, tough, it just
 * doesn't get there.
 *
 * First instinct was of course to just wait for however many milliseconds for
 * the popup window to spawn before sending it messages. But 200ms was not
 * enough time for this approach to work on my beast of a machine. Given that
 * our users are going to be running JR on normal machines, this approach is
 * simply infeasible.
 *
 * Instead after much denial and error, I found the approach where the good/bad page script
 * initiates a connection works consistently and is idiomatically sane, at
 * least in relative terms. In this case, we are using the Port abstraction
 * rather than the "one-off" connection thing that Mozilla says is low class.
 * It is better in this case so that we don't have to deal with process-level
 * global state in this function.
 *
 * If we wanted to interleave that nonsense into the global raseev loop at the
 * top of this file, we would have to basically have some registry of pages
 * we're trying to talk to, and then have some message queue that pairs each
 * message from a script with whomever is trying to listen to it.  That's
 * doable but I would prefer not to create global state if it's unnecessary.
 *
 * @internal
 */
async function
gb
    (title         : string,
     miscinfo      : string,
     timeout_iters : number)
    : Promise<gb>
{
    // TODO: the thread model here is a bit dubious... it's not 100% clear that
    // we're always going to be talking to the correct window. Do some
    // experiments.

    // we're going to make an unresolved result first
    // write our code to handle messages from the popup window
    // which updates this variable
    // once this variable is updated with resolved: true
    // then the entire function returns
    let the_result : {resolved : false}
                   | {resolved : true,
                      result   : gb}
                   = {resolved : false};
    // this is a closure that talks to the popup window from this context
    // it updates the_result when it gets something back from the user
    let port_talker_toer_lambda =
            function(port : browser.runtime.Port) {
                // @ts-ignore bad type info in the typedefs
                port.postMessage({title      : title,
                                  miscinfo   : miscinfo});

                // oh my god
                // nested lambdas
                // i think we've reached peak js
                let result_handler =
                        function(result_from_gb_popup_window : gb) {
                            console.error('RESULT FROM POPUP WINDOW!!!!', result_from_gb_popup_window);
                            // the message will either be good or bad
                            the_result = {resolved : true,
                                          result   : result_from_gb_popup_window};
                        };
                // @ts-ignore types from github are wrong
                port.onMessage.addListener(result_handler);
            }
    browser.runtime.onConnect.addListener(port_talker_toer_lambda);

    // does the user want to sign the message
    let confirm_window = await browser.windows.create({url  : '../pages/gb.html',
                                                       type : 'popup'});

    // @ts-ignore shut the fuck up
    let tabid : number = confirm_window.tabs[0].id;

    let this_iter_i1 : number = 1;
    let max_iters    : number = timeout_iters;

    // poor man's for loop because I just don't care
    // sleep for 5 milliseconds until resolved
    // this loop is guaranteed to terminate
    // and at its termination, the_result will have resolved:true
    while (!(the_result.resolved)) {
        // if we've timed out, time out
        if (this_iter_i1 > max_iters) {
            // the result is resolved and it's bad
            the_result = {resolved: true, result: 'bad'};
        }
        // in this case, we haven't timed out yet
        else {
            // so we simply increase the iteration
            this_iter_i1 = this_iter_i1 + 1;
            // sleep
            await sleep(GB_ITER_MS);
        }
    }

    // at this point, the_result is resolved:true
    // we know that the_result.result exists
    // typescript does not know that

    // close the tab
    browser.tabs.remove(tabid);

    // return the result
    // @ts-ignore i know this exists because I am jesus
    return the_result.result;
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
        console.log('jr state exists');
        // TS doesn't know we've proven the key exists and so therefore this is
        // of type bis_state
        return bi_s2i(gotten_state as bis_state);
    }
    // otherwise return default state
    else {
        console.log('jr no state... generating');
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



/**
 * Hack from stack overflow somewhere to sleep for the given number of ms
 */
async function
sleep
    (ms: number)
    : Promise<void>
{
    return new Promise(resolve => setTimeout(resolve, ms));
}

