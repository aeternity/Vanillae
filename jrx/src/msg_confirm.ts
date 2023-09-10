/**
 * Page script for "confirm sign message" popup window 
 */


main();

async function
main
    ()
    : Promise<void>
{
    console.log('msg_confirm main');
    var result = null;
    document.getElementById('good')!.onclick = () => { console.log('click good'); result = 'good' }
    document.getElementById('bad')!.onclick = () => { console.log('click bad'); result = 'bad' }

    console.log('adding listener');
    browser.runtime.onMessage.addListener(listener);
    console.log('added listener');
}

// for some reason the listener here never gets triggered
async function
listener
    (msg           : any,
     _sender       : any,
     _sendResponse : any)
    : Promise<void>
{
    console.log('listener triggered');
    console.log(msg);
}
