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

    let result : '' | 'good' | 'bad'
               = '';

    document.getElementById('good')!.onclick
        = function() {
            console.log('click good');
            result = 'good';
        };
    document.getElementById('bad')!.onclick
        = function() {
            console.log('click bad');
            result = 'bad';
        };


    type bg_msg
        = {tx_str  : string,
           tx_data : object};

    async function listener
        (msg           : bg_msg,
         _sender       : any,
         _sendResponse : any)
        : Promise<'good' | 'bad'>
    {
        console.log('listener triggered', msg);
        console.log('tx_str:', msg.tx_str);
        console.log('tx_data:', msg.tx_data);
        document.getElementById('tx-base64')!.innerHTML = msg.tx_str;
        document.getElementById('tx-decomposed')!.innerHTML = JSON.stringify(msg.tx_data, undefined, 4);

        // every 5 ms check
        // timeout of 10 minutes = 10*60 secs * 20 iterations = 
        // number of iters in a full second
        let ITERS = 1;
        let SEC   = 200*ITERS;
        let MIN   = 60*SEC;
        //let n_max = 10*MIN;
        let n     = 1;
        let n_max = 30*MIN;

        // result starts as neutral
        // if user clicks good, result will be updated to 'good'
        // if user clicks bad, result will be updated to 'bad'
        // if user doesn't click in 30 seconds, default to 'bad'
        // this has the effect of sitting and waiting until the user clicks a button
        // with a timeout as a backstop
        while
        (result === '') {
            // if haven't timed out yet
            if (n <= n_max) {
                await sleep(5);
                n = n + 1;
            }
            // if timed out
            else {
                result = 'bad';
                // this break is implied but you're not smart enough to figure
                // that out yourself
                break;
            }
        }

        // send result back
        return result;
    }

    // add listener
    browser.runtime.onMessage.addListener(listener);
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
