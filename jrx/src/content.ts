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


jr_content_main();


async function
jr_content_main
    ()
{
   // @ts-ignore browser
   browser.runtime.onMessage.addListener(handler);
   window.addEventListener('message', page_script_message_handler);




    /**
     * This is the message that we spam the page script with when the user clicks
     * the "make wallet detectable" button.
     *
     * Contains {@link detect_msg} as a field
     */
    let detect_awcp_msg = {type : "to_aepp",
                           data : {jsonrpc : "2.0",
                                   method  : "connection.announcePresence",
                                   params  : detect_msg()}};

    // mk detectable
    mk_detectable(detect_awcp_msg);
}
