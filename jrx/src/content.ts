/**
 * JR Content Script
 *
 * Notes:
 *
 * 1. This is not loaded as a module, which means it has limited permissions.
 *    In particular, asynchronous calls are 1000x more annoying.
 * 2. All this does is relay messages between here and the background script
 *    (i.e. application controller).
 *
 * @module
 */



let detect_msg = {type : "to_aepp",
                  data : {jsonrpc : "2.0",
                          method  : "connection.announcePresence",
                          params  : {id        : "jr",
                                     name      : "JR",
                                     networkId : "ae_uat",
                                     origin    : "foobar",
                                     type      : "extension"}}};

/**
 * Spam detect message
 */
async function
spam_detect
    ()
{
    while (true) {
        window.postMessage(detect_msg);
        await sleep(3000);
    }
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



/**
 * Relays messages from page scripts to the background script
 */
async function
a2w_handler
    (msg: {data: {type? : "to_aepp" | "to_waellet"}})
{

    //console.error('JR: a2w_handler', {msg: msg});
    function onSuccessCase(response_from_bg: any) {
        console.error('JR A2w success case', response_from_bg);
        window.postMessage(response_from_bg);
    }
    function onErrorCase(error: any) {
        console.error('JR: error from controller', error);
    }

    // branch
    if ("to_waellet" === msg.data.type) {
        console.error('JR: WAEEEEEEE');
        browser.runtime.sendMessage(msg.data).then(
            onSuccessCase,
            onErrorCase
        );
        //console.error('BOOBS', response);
        //window.postMessage(response);
    }
    // otherwise ignore
}



///**
// * Relays messages from background scripts to the page script
// */
//function
//w2a_handler
//    (msg: any)
//{
//    window.postMessage(msg);
//}



/**
 * Main function
 */
async function
jr_content_main
    ()
{

   // relay page messages back to the controller
   window.addEventListener('message', a2w_handler);

   //// relay controller messages back to page
   //browser.runtime.onMessage.addListener(w2a_handler);

   // spam detect
   spam_detect();
}


jr_content_main();
