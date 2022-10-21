/**
 * # How to use this library
 *
 * This is a library for communicating with a browser wallet extension such as
 * Superhero
 *
 * ## Step 0: Include `sidekick`
 *
 * ```
 * import * as sk from './path/to/sidekick.js';
 * ```
 *
 * ## Step 1: Make a `Logger`
 *
 * All of the entrypoints in sidekick require passing in a `Logger`. The idea
 * is that you can pass in custom logging hooks to log potential errors.
 *
 * There are two built-in loggers exported by this module:
 *
 * 1.  `let my_logger = sk.wsl();`: does nothing
 * 2.  `let my_logger = sk.cl();`: console logger
 * 3.  `let my_logger = new sk.HttpLogger('https://foo.bar/baz')`: sends JSON to
 *     the given endpoint in a POST request, in the following form
 *
 *     ```
 *     {level   : 'debug' | 'info' | 'warning' | 'error',
 *      message : string,
 *      data    : object}
 *     ```
 * 4.  `let my_logger = new sk.SeqLogger([my_logger1, my_logger2]);`: a helper
 *     for composing several loggers sequentially.
 * 5.  You can define anything that satisfies the `Logger` interface and pass
 *     that in instead.
 *
 * ```
 * interface Logger {
 *     debug   : (message : string, data : object) => Promise<void>;
 *     info    : (message : string, data : object) => Promise<void>;
 *     warning : (message : string, data : object) => Promise<void>;
 *     error   : (message : string, data : object) => Promise<void>;
 * }
 * ```
 *
 * ## Step 2: Detect the wallet
 *
 * ```
 * let maybe_detected = await sk.detect(sk.TIMEOUT_DEF_DETECT, 'detect: timeout', my_logger);
 * ```
 *
 * Function:
 *
 * ```
 * async function
 * detect
 *     (timeout_ms  : number,
 *      timeout_msg : string,
 *      logger      : Logger)
 *     : Promise<Safe<awcp.Params_W2A_connection_announcePresence, SkTimeoutError>>
 * ```
 *
 * This returns some garbage that doesn't matter in a `Safe` type.
 * The `Safe` type does matter
 *
 * ```
 * type Safe<ok_t, err_t>
 *     = Ok<ok_t>
 *     | Error<err_t>;
 *
 * type Ok<ok_t>
 *     = {ok     : true,
 *        result : ok_t};
 *
 * type Error<err_t>
 *     = {ok    : false,
 *        error : err_t};
 * ```
 *
 * The motivation here is that when talking to the wallet, there are many
 * possible sources of errors.  For instance, if you ask the wallet to sign a
 * transaction, the transaction might be malformed, maybe the user declines,
 * maybe it times out, whatever. All you care about is "did it work?" and you
 * don't want to deal with try/catch bullshit.
 *
 * The most straightforward way to extract the return value is with branching:
 *
 * ```
 * if (maybe_detected.ok) {
 *     // ok is true in this case, so the field `result` exists
 *     let awcp_crap = maybe_detected.result;
 * }
 * else {
 *     // ok is false in this case, so the field `error` exists
 *     let the_error = maybe_detected.error;
 * }
 * ```
 *
 * ## Step 3: Connect to the wallet
 *
 *
 *
 * ## Step 4: Get the user address
 *
 * ## Step 5: Sign a transaction
 *
 *
 * @module
 */

// TODONE: add standardized logging interface
// TODONE: logging hooks
// TODO: invoice
// TODO: get connect done
// TODO: make the message queue for responses a map, fill the message queue properly
// TODO: get it working with superhero
// TODO: jrx

// like: console, http, etc
// allow someone to pass a logger


//-----------------------------------------------------------------------------
// EXPORTS
//-----------------------------------------------------------------------------

export {
    // debugging
        hello,
    // computational dental dams
        Safe,
        unsafe,
        ok,
        error,
    // timeout errors
        ERROR_CODE_SkTimeoutError,
        SkTimeoutError,
        sk_timeout,
    // logging bullshit
        Logger,
        WebScale,
        wsl,
        ConsoleLogger,
        cl,
        HttpLogger,
        http_log,
        SeqLogger,
        logger_foreach,
    // timeouts
        // time constants
        MS,
        SEC,
        MIN,
        HR,
        // timeouts
        TIMEOUT_DEF_DETECT_MS,
        TIMEOUT_DEF_CONNECT_MS,
        TIMEOUT_DEF_ADDRESS_MS,
        TIMEOUT_DEF_TX_SIGN_NOPROP_MS,
    // API
        // detect
        detect,
        CAPListener,
        // connect
        connect,
        address,
        tx_sign_noprop,

    // internals
        sleep,
        bulrtot
};


//-----------------------------------------------------------------------------
// IMPORTS
//-----------------------------------------------------------------------------

import * as awcp from './jex_include/local-awcp-0.1.0/dist/awcp.js';


//-----------------------------------------------------------------------------
// API
//-----------------------------------------------------------------------------

/**
 * Use for debugging/to check if import worked correctly
 */
function
hello
    ()
    : void
{
    console.log('hællo');
}



/**
 * Type that catches positive errors
 */
type Safe<ok_t, err_t>
    = Ok<ok_t>
    | Error<err_t>;



/**
 * Ok type
 */
type Ok<ok_t>
    = {ok     : true,
       result : ok_t};



/**
 * Err type
 */
type Error<err_t>
    = {ok    : false,
       error : err_t};


/**
 * Constructs an `Ok` value from a pure value
 */
function
ok
    <ok_t>
    (x : ok_t)
    : Ok<ok_t>
{
    return {ok: true, result: x};
}



/**
 * Constructs an `Error` value from a pure value
 */
function
error
    <err_t>
    (x: err_t)
    : Error<err_t>
{
    return {ok: false, error: x};
}



/**
 * Takes a `Safe` value, if `ok`, returns the `ok_t`, or if an error throws the
 * `err_t`
 */
function
unsafe
    <ok_t, err_t>
    (x: Safe<ok_t, err_t>)
    : ok_t
{
    if (x.ok)
        return x.result;
    else
        throw x.error;
}



/** Error code for `SkTimeoutError`s */
const ERROR_CODE_SkTimeoutError = 420;


/**
 * Timeout Error
 */
type SkTimeoutError
    = {code    : 420,
       message : string,
       data    : object};



/**
 * Construct a `SkTimeoutError`
 */
function
sk_timeout
    (message : string,
     data    : object)
{
    return {code    : 420 as 420,   // typescript is great
            message : message,
            data    : data};
}

/**
 * It's web scale
 */
function wsl() { return new WebScale(); }

/** construct a `ConsoleLogger` */
function cl() { return new ConsoleLogger(); }


/**
 * Callbacks you need to implement if you want logging
 */
interface Logger
{
    debug   : (message : string, data : object) => Promise<void>;
    info    : (message : string, data : object) => Promise<void>;
    warning : (message : string, data : object) => Promise<void>;
    error   : (message : string, data : object) => Promise<void>;
}

/**
 * Web scale
 */
class WebScale implements Logger
{
    async debug   (_msg: string, _data: object) { return; }
    async info    (_msg: string, _data: object) { return; }
    async warning (_msg: string, _data: object) { return; }
    async error   (_msg: string, _data: object) { return; }
}

/**
 * does console.log
 */
class ConsoleLogger implements Logger
{
    listener : (e : Event) => void;
    constructor() {
        let this_ptr = this;
        this.listener =
                function (e : Event) {
                    // for typescript
                    if (e instanceof MessageEvent) {
                        this_ptr.debug(`ConsoleLogger received MessageEvent`, e.data);
                    }
                };
    }
    async debug   (_msg: string, _data: object) { console.debug (_msg, _data); }
    async info    (_msg: string, _data: object) { console.log   (_msg, _data); }
    async warning (_msg: string, _data: object) { console.warn  (_msg, _data); }
    async error   (_msg: string, _data: object) { console.error (_msg, _data); }
    listen() {
        window.addEventListener('message', this.listener);
    }
    ignore () {
        window.removeEventListener('message', this.listener);
    }
}

/**
 * posts request to an HTTP server
 */
class HttpLogger implements Logger
{
    post_endpoint: string;
    listener : (e : Event) => void;
    constructor (post_endpoint: string) {
        this.post_endpoint = post_endpoint;
        let this_ptr = this;
        this.listener =
                function (e : Event) {
                    // for typescript
                    if (e instanceof MessageEvent) {
                        this_ptr.debug(`HttpLogger received MessageEvent`, e.data);
                    }
                };
    }
    async debug   (_msg: string, _data: object) { http_log(this.post_endpoint, 'debug', _msg, _data); }
    async info    (_msg: string, _data: object) { http_log(this.post_endpoint, 'info', _msg, _data); }
    async warning (_msg: string, _data: object) { http_log(this.post_endpoint, 'warning', _msg, _data); }
    async error   (_msg: string, _data: object) { http_log(this.post_endpoint, 'error', _msg, _data); }
    listen() {
        window.addEventListener('message', this.listener);
    }
    ignore () {
        window.removeEventListener('message', this.listener);
    }
}

async function
http_log
    (post_endpoint  : string,
     log_level      : 'debug' | 'info' | 'warning' | 'error',
     message        : string,
     data           : object)
    : Promise<void>
{
    try
    {
        await fetch(post_endpoint, {method : 'POST',
                                    body   : JSON.stringify({level: log_level,
                                                             message: message,
                                                             data: data},
                                                            undefined,
                                                            4)});
    }
    catch (e)
    {
        console.error(e);
    }
}

/**
 * have an array of loggers fire sequentially
 */
class SeqLogger implements Logger
{
    loggers: Array<Logger>
    constructor (loggers : Array<Logger>) { this.loggers = loggers; }
    async debug   (_msg: string, _data: object) { logger_foreach(this.loggers, 'debug'  , _msg, _data); }
    async info    (_msg: string, _data: object) { logger_foreach(this.loggers, 'info'   , _msg, _data); }
    async warning (_msg: string, _data: object) { logger_foreach(this.loggers, 'warning', _msg, _data); }
    async error   (_msg: string, _data: object) { logger_foreach(this.loggers, 'error'  , _msg, _data); }
}

async function
logger_foreach
    (loggers   : Array<Logger>,
     log_level : 'debug' | 'info' | 'warning' | 'error',
     message   : string,
     data      : object)
    : Promise<void>
{
    for (let logger of loggers)
    {
        switch(log_level)
        {
            case 'debug':
                logger.debug(message, data);
                break;
            case 'info':
                logger.info(message, data);
                break;
            case 'warning':
                logger.warning(message, data);
                break;
            case 'error':
                logger.error(message, data);
                break;
        }
    }
}

//-----------------------------------------------------------------------------
// ACTUAL API
//-----------------------------------------------------------------------------

// UNITS OF TIME

/** Unit of time; `const MS = 1` */
const MS  = 1;
/** `const SEC = 1000*MS` */
const SEC = 1000*MS;
/** `const MIN = 1000*MS` */
const MIN = 60*SEC;
/** `const HR = 60*MIN` */
const HR = 60*MIN;

// TIMEOUTS

/**
 * 7 seconds. Superhero announces itself every 3 seconds, so this is 2
 * announcements + 1 second.
 */
const TIMEOUT_DEF_DETECT_MS  = 7*SEC;

/**
 * 1 second (instaneous in practice)
 */
const TIMEOUT_DEF_CONNECT_MS = 1*SEC;


/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is where you as the developer
 * need to exercise some discretion.
 */
const TIMEOUT_DEF_ADDRESS_MS = 5*MIN;


/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is an instance where you as
 * the developer need to exercise some discretion.
 */
const TIMEOUT_DEF_TX_SIGN_NOPROP_MS = 5*MIN;


// Ah ok
//
// so for connection.announcePresence, we just listen and recieve, unwrap, send back
//
// for everything else, we send a request, await a response
//
// either: SkTimeoutError or an RpcError



//-----------------------------------------------------------------------------
// API: detection
//-----------------------------------------------------------------------------

/**
 * Wait for wallet to announce itself, and then return.
 *
 * Example message data:
 *
 * ```json
 * {
 *     "type": "to_aepp",
 *     "data": {
 *         "jsonrpc": "2.0",
 *         "method": "connection.announcePresence",
 *         "params": {
 *             "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
 *             "name": "Superhero",
 *             "networkId": "ae_mainnet",
 *             "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
 *             "type": "extension"
 *         }
 *     }
 * }
 * ```
 *
 * Example return data:
 *
 * ```json
 * {
 *     "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
 *     "name": "Superhero",
 *     "networkId": "ae_mainnet",
 *     "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
 *     "type": "extension"
 * }
 * ```
 *
 * Example usage:
 *
 * ```typescript
 * let wallet_info = await sk.detect(sk.TIMEOUT_DEF_DETECT_MS, "wallet timed out", sk.cl());
 * ```
 */
async function
detect
    (timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Params_W2A_connection_announcePresence, SkTimeoutError>>
{
    let call_params = {timeout_ms   : timeout_ms,
                       timeout_msg  : timeout_msg};
    logger.debug('detect', call_params);
    let listener = new CAPListener(logger);
    logger.debug('detect: listening on window', {});
    listener.listen();
    let result = await listener.raseev(timeout_ms, timeout_msg);
    logger.debug('detect: result', {result:result});
    logger.debug('detect: ignoring window', {});
    listener.ignore();
    return result;
}



/**
 * Listens for `connection.announcePresence` messages
 *
 * @internal
 */
class CAPListener
{
    logger    : Logger;
    listener  : ((e : Event) => void );
    cap_queue : null | awcp.Params_W2A_connection_announcePresence = null;

    constructor(logger: Logger) {
        logger.debug('CAPListener.constructor', {});
        this.logger = logger;
        // js pointer hack
        const this_ptr = this;
        this.listener = function (event : Event) { this_ptr.handle(event); };
    }


    listen() : void {
        this.logger.info('CAPListener.listen', {});
        window.addEventListener('message', this.listener);
    }


    ignore() : void {
        this.logger.info('CAPListener.ignore', {});
        window.removeEventListener('message', this.listener);
    }


    handle(evt : Event) : void {
        this.logger.debug('CAPListener.handle', {event: evt});
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }

    really_handle
        (evt : MessageEvent<any>)
        : void
    {
        this.logger.debug('CAPListener.really_handle', {event: evt});
        // Example message data:
        //
        // ```json
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": "connection.announcePresence",
        //         "params": {
        //             "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //             "name": "Superhero",
        //             "networkId": "ae_mainnet",
        //             "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //             "type": "extension"
        //         }
        //     }
        // }
        // ```
        //
        // Example queue data:
        //
        // ```json
        // {
        //     "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //     "name": "Superhero",
        //     "networkId": "ae_mainnet",
        //     "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //     "type": "extension"
        // }
        // ```
        let queue_empty   : boolean = !this.cap_queue;
        let msg_is_for_us : boolean = evt.data.type === "to_aepp";
        let is_cap_msg    : boolean = evt.data.data.method === "connection.announcePresence";
        let we_rollin     : boolean = queue_empty && msg_is_for_us && is_cap_msg;

        this.logger.debug('CAPListener.really_handle: branching variables regarding how to handle this event',
                          {queue_empty   : queue_empty,
                           msg_is_for_us : msg_is_for_us,
                           is_cap_msg    : is_cap_msg,
                           we_rollin     : we_rollin,
                           event         : evt});
        if (we_rollin) {
            this.logger.debug('CAPListener.really_handle: queue empty, message is for us, and it is the message we want, so adding to queue',
                              {event: evt,
                               new_queue: evt.data.data.params});
            this.cap_queue = evt.data.data.params;
        }
        else {
            this.logger.debug("CAPListener.really_handle: for whatever reason, we're ignoring this event",
                              {event: evt});
        }
    }


    async raseev
        (timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<awcp.Params_W2A_connection_announcePresence, SkTimeoutError>>
    {
        this.logger.debug('CAPListener.raseev',
                           {timeout_ms  : timeout_ms,
                            timeout_msg : timeout_msg});
        // stupid js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock =
                function () {
                    // this means queue is not empty
                    return !!(this_ptr.cap_queue);
                };

        let get_result =
                function () {
                    this_ptr.logger.debug('CAPListener.raseev.get_result', {});
                    return this_ptr.cap_queue as awcp.Params_W2A_connection_announcePresence;
                };
        let result =
                await bulrtot<awcp.Params_W2A_connection_announcePresence>
                             (lambda_that_must_return_true_to_unblock,
                              get_result,
                              timeout_ms,
                              timeout_msg,
                              this.logger);
        return result;
    }
}



//-----------------------------------------------------------------------------
// API: connection
//-----------------------------------------------------------------------------

async function
connect
    (id          : number | string,
     params      : awcp.Params_A2W_connection_open,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_connection_open, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('connect', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"connection.open", awcp.Params_A2W_connection_open, awcp.Result_W2A_connection_open>
                (id, "connection.open", params, timeout_ms, timeout_msg);
    return result;
}

//-----------------------------------------------------------------------------
// API: get address
//-----------------------------------------------------------------------------

async function
address
    (id          : number | string,
     params      : awcp.Params_A2W_address_subscribe,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_address_subscribe, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('address', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"address.subscribe", awcp.Params_A2W_address_subscribe, awcp.Result_W2A_address_subscribe>
                (id, "address.subscribe", params, timeout_ms, timeout_msg);
    return result;
}

//-----------------------------------------------------------------------------
// API: tx sign (no prop)
//-----------------------------------------------------------------------------

async function
tx_sign_noprop
    (id          : number | string,
     params      : awcp.Params_A2W_tx_sign_noprop,
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<awcp.Result_W2A_tx_sign_noprop, awcp.RpcError | SkTimeoutError>>
{
    logger.debug('address', {id:id, params:params, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result =
        await msgr.send_raseev
                <"transaction.sign", awcp.Params_A2W_tx_sign_noprop, awcp.Result_W2A_tx_sign_noprop>
                (id, "transaction.sign", params, timeout_ms, timeout_msg);
    return result;
}



class MsgR {
    logger    : Logger;
    listener  : ((e : Event) => void );
    // FIXDME: make this a map
    queue     : Map<string|number, object> = new Map();

    constructor (logger : Logger) {
        this.logger = logger;
        this.logger.debug('MsgR.constructor', {});
        const this_ptr = this;
        this.listener = function (event : Event) { this_ptr.handle(event); }
    }
    listen () : void {
        this.logger.debug('MsgR.listen', {});
        window.addEventListener('message', this.listener);
    }
    ignore () : void {
        this.logger.debug('MsgR.ignore', {});
        window.removeEventListener('message', this.listener);
    }
    handle (evt : Event) : void {
        this.logger.debug('MsgR.handle', {event:evt});
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }
    really_handle (evt : MessageEvent<any>) {
        this.logger.debug('MsgR.really_handle', {event:evt});
        // message is
        //
        // raseeving based on id
        //
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": don't care,
        //         "id": the key
        //     }
        // }
        //
        // value is the entire message data
        // is it for us, and does it have an id field
        if ((evt.data.type === "to_aepp") && !!(evt.data.data.id))
        {
            // now we know it's a message for us
            let key = evt.data.data.id;
            let val = evt.data;
            this.queue.set(key, val);
        }
    }
    async send_raseev
        <method_s extends string,
         params_t extends object,
         result_t extends any>
        (id          : number | string,
         method      : method_s,
         params      : params_t,
         timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<result_t, SkTimeoutError | awcp.RpcError>>
    {
        this.logger.debug('MsgR.send_and_raseev',
                          {id          : id,
                           method      : method,
                           params      : params,
                           timeout_ms  : timeout_ms,
                           timeout_msg : timeout_msg});
        // make the message
        let window_msg = mk_window_msg(id, method, params);
        this.logger.debug('MsgR.send_and_raseev posting message', {window_msg: window_msg});
        // listen
        this.listen();
        // send the message
        window.postMessage(window_msg);
        // receive reply
        let response: Safe<awcp.EventData_W2A<awcp.RpcResp<method_s, result_t>>, SkTimeoutError> =
                await this.raseev(id, timeout_ms, timeout_msg);
        // unwrap the rpc jizz
        let result: Safe<result_t, awcp.RpcError | SkTimeoutError> =
                Safe_AWCP_W2A_Msg_to_Safe_result(response);
        // ignore target
        this.ignore();
        return result;
    }
    async raseev
        <result_t extends object>
        (id          : string | number,
         timeout_ms  : number,
         timeout_msg : string)
        : Promise<Safe<result_t, SkTimeoutError>>
    {
        this.logger.debug('MsgQ.raseev', {id:id, timeout_ms:timeout_ms, timeout_msg:timeout_msg});
        // js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock = function () { return this_ptr.queue.has(id); };
        let result_fun = function () { return (this_ptr.queue.get(id) as result_t); };
        let result = await bulrtot(lambda_that_must_return_true_to_unblock,
                                   result_fun,
                                   timeout_ms,
                                   timeout_msg,
                                   this.logger);
        return result;
    }
}


function
mk_window_msg
    <method_s extends string,
     params_t extends object>
    (id     : number | string,
     method : method_s,
     params : params_t)
    : awcp.EventData_A2W<awcp.RpcCall<method_s, params_t>>
{
    let rpc_message : awcp.RpcCall<method_s, params_t> =mk_rpc_message(id, method, params);
    return {type: "to_waellet",
            data: rpc_message};
}

function
mk_rpc_message
    <method_s extends string,
     params_t extends object>
    (id     : number | string,
     method : method_s,
     params : params_t)
    : awcp.RpcCall<method_s, params_t>
{
    return {jsonrpc : "2.0",
            id      : id,
            method  : method,
            params  : params};
}


///**
// * Returns the value of `dispatchEvent`
// *
// * See https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/dispatchEvent
// */
//function
//send
//    <data_t extends any>
//    (tgt  : EventTarget & MessageEventSource,
//     data : data_t)
//    : boolean
//{
//    // TODO: look at options besides `data`
//    // See: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/MessageEvent
//    let msg_event: MessageEvent<data_t> = new MessageEvent('message', {data: data, source: tgt});
//    let result = tgt.dispatchEvent(msg_event);
//    return result;
//}
//

//-----------------------------------------------------------------------------
// INTERNALS
//-----------------------------------------------------------------------------


/**
 * Stack overflow: https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
 *
 * No fucking idea what's going on here
 *
 * Some crazy async hack bullshit
 *
 * It works, who cares
 *
 * @internal
 */
async function
sleep
    (ms : number)
{
    return new Promise(resolve => setTimeout(resolve, ms));
}



/**
 * Block until lambda returns true or timeout
 *
 * `timeout_ms` should be divisible by `50`
 *
 * @internal
 */
async function
bulrtot
    <ok_t>
    (fun         : (() => boolean),
     result_fun  : (() => ok_t),
     timeout_ms  : number,
     timeout_msg : string,
     logger      : Logger)
    : Promise<Safe<ok_t, SkTimeoutError>>
{
    logger.debug('bulrtot (block until lambda returns true or timeout)',
                 {timeout_ms  : timeout_ms,
                  timeout_msg : timeout_msg});
    let max_iters : number = Math.floor(timeout_ms / 50);
    logger.debug('bulrtot: iterating every 50 milliseconds',
                 {max_iters: max_iters});

    for(let i = 1; i <= max_iters; i++)
    {
        if (fun())
        {
            logger.debug('bulrtot: lambda returned true on i-th iteration',
                         {i:i, max_iters:max_iters});
            let result = ok(result_fun());
            logger.debug('bulrtot: result (ok)',
                         {result: result});
            return result;
        }
        else
            await sleep(50);
    }

    logger.debug('bulrtot: max iterations exceeded', {max_iters:max_iters});
    let result = sk_timeout(timeout_msg, {});
    logger.debug('bulrtot: result (error)', {result: result});
    return error(result);
}



/**
 * Converts a `Safe`-wrapped `RpcResp` (which may be a success/error) to a
 * `Safe` value
 *
 * Errors can be generated from one of two places: from the wallet (which
 * encodes it in the RPC response), or from sidekick via a timeout.
 *
 * Basically, when we call `bulrtot`, we're going to get back a `Safe`-wrapped
 * `awcp.RpcResp`, which may be success value or an error value.
 *
 * In the timeout error case, preserve.
 *
 * In the case of RPC success, this unwraps whatever is in the `RpcResp`.
 *
 * In the case of RPC failure, this unwraps the error.
 *
 * @internal
 */
function
Safe_AWCP_W2A_Msg_to_Safe_result
    <method_s extends string,
     success_t extends any>
    (safe_w2a_msg : Safe<awcp.EventData_W2A<awcp.RpcResp<method_s, success_t>>, SkTimeoutError>)
    : Safe<success_t,
           awcp.RpcError | SkTimeoutError>
{
    // if input is a success (i.e. NOT a Timeout error), branch on if it's an rpc
    // error
    if (safe_w2a_msg.ok)
    {
        // we have 
        // {ok: true, result: {type: "to_aepp", data: rpc jizz}}
        // want to pull out the rpc jizz
        // case split on whether or not there was an RPC error (i.e. an error generated by the wallet)
        // then our top level return is a safety-wrapped error
        // which is either {ok, TheActualResultWeWant} or {error, RpcError}
        // (this branch of the if) or {error, TimeoutError} (the else branch)
        let ok_w2a_msg = safe_w2a_msg.result;
        let rpc_resp: awcp.RpcResp<method_s, success_t> = ok_w2a_msg.data;

        // From AWCP:
        // /**
        //  * This is the shape of unsuccessful responses
        //  */
        // type RpcResp_error
        //     <method_s extends string>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        error   : RpcError};
        // /**
        //  * This is the shape of successful responses
        //  */
        // type RpcResp_ok
        //     <method_s extends string,
        //      result_t extends any>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        result  : result_t};
        // /**
        //  * This is the shape of generic responses
        //  */
        // type RpcResp
        //     <method_s extends string,
        //      result_t extends any>
        //     = RpcResp_ok<method_s, result_t>
        //     | RpcResp_error<method_s>;

        // so this may be an RpcResp_ok or an RpcResp_error
        // the next line figures that out
        // @ts-ignore typescript is mad because the property that i'm testing to see if it exists might not exist
        let rpc_resp_is_ok: boolean = !!(rpc_resp.result);
        // branch on if we're an RpcResp_ok or an RpcResp_error
        if (rpc_resp_is_ok) {
            // ok so here we're in the RpcResp_ok branch
            // the `result` field exists and we have it
            let the_actual_result: success_t = (rpc_resp as awcp.RpcResp_ok<string, success_t>).result;
            return ok(the_actual_result);
        }
        // error case:
        else {
            let the_error: awcp.RpcError = (rpc_resp as awcp.RpcResp_error<string>).error;
            return error(the_error);
        }
    }
    // this is the timeout error case
    //
    // in which case, the result is what we want
    else
    {
        return safe_w2a_msg;
    }
}
