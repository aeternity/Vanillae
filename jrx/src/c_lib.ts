/**
 * JR Content Script Library
 *
 * Notes:
 *
 * 1. This is a module.
 * 2. All this does is relay messages between here and the background script
 *    (i.e. application controller).
 *
 * @module
 */


export {
    c_main
};

/**
 * Main function
 */
async function
c_main
    ()
{
    console.log('c_main');

    //// relay page messages back to the controller
    //window.addEventListener('message', a2w_handler);

    //// spam detect
    //spam_detect();
}
//
//
//
//let detect_msg = {type : "to_aepp",
//                  data : {jsonrpc : "2.0",
//                          method  : "connection.announcePresence",
//                          params  : {id        : "jr",
//                                     name      : "JR",
//                                     networkId : "ae_uat",
//                                     origin    : "foobar",
//                                     type      : "extension"}}};
//
///**
// * Spam the "connection.announcePresence" (awcp terminology: "detect") message
// */
//async function
//spam_detect
//    ()
//{
//    while (true) {
//        window.postMessage(detect_msg);
//        await sleep(3000);
//    }
//}
//
//
//
///**
// * Hack from stack overflow somewhere to sleep for the given number of ms
// */
//async function
//sleep
//    (ms: number)
//    : Promise<void>
//{
//    return new Promise(resolve => setTimeout(resolve, ms));
//}
//
//
//
///**
// * Relays messages from page scripts to the background script
// */
//async function
//a2w_handler
//    (msg: {data: {type? : "to_aepp" | "to_waellet"}})
//{
//
//    //console.error('JR: a2w_handler', {msg: msg});
//    function onSuccessCase(response_from_bg: any) {
//        console.error('JR A2w success case', response_from_bg);
//        window.postMessage(response_from_bg);
//    }
//    function onErrorCase(error: any) {
//        console.error('JR: error from controller', error);
//    }
//
//    // branch
//    // all messages are visible to us
//    // if the message is for the wallet
//    // send it to the wallet
//    // otherwise ignore it
//    if ("to_waellet" === msg.data.type) {
//        console.error('JR: WAEEEEEEE');
//        browser.runtime
//            .sendMessage({frum: 'content',
//                          data: msg.data})
//            .then(onSuccessCase,
//                  onErrorCase);
//        //window.postMessage(response);
//    }
//    // otherwise ignore
//}
//
//
//
/////**
//// * Relays messages from background scripts to the page script
//// */
////function
////w2a_handler
////    (msg: any)
////{
////    window.postMessage(msg);
////}
//
