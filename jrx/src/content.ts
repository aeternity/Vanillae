/**
 * JR Content Script
 *
 * Notes:
 *
 * 1. This is not a module, so calling async code is super annoying and
 *    everything has to be in script order.
 * 2. All this does is relay messages between here and the background script
 *    (i.e. application controller).
 *
 * This is more or less a finished script. Doesn't need the "api call" idiom
 * that the popup <-> controller IPC needs.
 *
 * @module
 */


let detect_msg = {type : "to_aepp",
                  data : {jsonrpc : "2.0",
                          method  : "connection.announcePresence",
                          params  : {id        : "jr",
                                     name      : "JR",
                                     networkId : "ae_uat",
                                     origin    : browser.runtime.getURL('/'),
                                     type      : "extension"}}};

/**
 * Spam the "connection.announcePresence" (awcp terminology: "detect") message
 */
async function
c_spam_detect
    ()
{
    while (true) {
        window.postMessage(detect_msg);
        await c_sleep(3000);
    }
}



/**
 * Hack from stack overflow somewhere to sleep for the given number of ms
 */
async function
c_sleep
    (ms: number)
    : Promise<void>
{
    return new Promise(resolve => setTimeout(resolve, ms));
}



/**
 * Relays messages between page scripts and the background script
 */
async function
c_a2w_handler
    (msg: {data: {type? : "to_aepp" | "to_waellet"}})
    : Promise<void>
{

    //console.error('JR: a2w_handler', {msg: msg});
    function onSuccessCase(response_from_bg: any) {
        console.log('jr: c_a2w_handler success ', response_from_bg);
        window.postMessage(response_from_bg);
    }
    function onErrorCase(error: any) {
        console.error('jr: c_a2w_handler: error from controller', error);
    }

    // branch
    // all messages are visible to us
    // if the message is for the wallet
    // send it to the wallet
    // otherwise ignore it
    if ("to_waellet" === msg.data.type) {
        console.log('jr: c_a2w_handler relaying message', msg.data);
        browser.runtime
            .sendMessage({frum: 'content',
                          data: msg.data})
            .then(onSuccessCase,
                  onErrorCase);
    }
    // otherwise ignore
}


/**
 * Main function
 */
async function
c_main
    ()
{
    console.log('c_main');

    // relay messages back and forth between page script and controller
    window.addEventListener('message', c_a2w_handler);

    // spam the detect message
    c_spam_detect();
}

c_main()
