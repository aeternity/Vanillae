/**
 * Page script for "confirm sign message" popup window 
 *
 * For some reason getting "duplicate function implementation" errors from TSC
 * if I give these functions normal names
 *
 * Getting this error on both this file and tx_confirm.ts
 *
 * For some reason changing the names only in this file gets rid of the errors
 * in both files.
 *
 * No idea what the fuck is going on and I can almost guarantee I don't want to
 * know.
 *
 * Everything about this language is painful. You know I think a lot. I wonder
 * why everything is stupid. In any individual case, you can understand the
 * chain of incentives for why thing X is stupid. But the majority of things
 * should not be stupid. The stupid should be the exception. Instead the world
 * runs on the short bus tech stack.
 */


maine();


async function
maine
    ()
    : Promise<void>
{
    console.log('msg_confirm maine');

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
        = {msg_str : string};

    async function listener
        (msg           : bg_msg,
         _sender       : any,
         _sendResponse : any)
        : Promise<'good' | 'bad'>
    {
        console.log('listener triggered', msg);
        console.log('msg_str:', msg.msg_str);
        document.getElementById('message')!.innerHTML = msg.msg_str;

        // every 5 ms check
        // timeout of 10 minutes = 10*60 secs * 20 iterations = 
        // number of iters in a full second
        let ITERS = 1;
        let SEC   = 200*ITERS;
        let MIN   = 60*SEC;
        //let n_max = 10*MIN;
        let n     = 1;
        let n_max = 30*SEC;

        while
        (result === '') {
            // if haven't timed out yet
            if (n <= n_max) {
                await sleepy(5);
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
sleepy
    (ms: number)
    : Promise<void>
{
    return new Promise(resolve => setTimeout(resolve, ms));
}
