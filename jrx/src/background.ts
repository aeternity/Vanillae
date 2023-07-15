/**
 * JR Controller
 *
 * Note that this is a *thread*.  We put our business and storage logic within
 * this thread.  The reason we do that is we don't want to deal with the
 * problem of conflicting concurrent writes..
 *
 * @module
 */


/**
 * Handle messages from the content script
 */
async function
bg_a2w_handler
    (msg: any, sender: any, sendResponse: any)
    : Promise<void>
{
    console.log('msg', msg);
    console.log('sender', sender);
    console.log('sendResponse', sendResponse);
    // send them back to content script
    // @ts-ignore stahp
    //let the_sender: number = sender.tab.id;
    //browser.tabs.sendMessage(the_sender, msg);
    // @ts-ignore stahp
    //sendResponse(msg);
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
