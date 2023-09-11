/**
 * Missing routines
 *
 * @module
 */

export {
    sleep,
    bulrtot
}

import * as vdk_safe from './jex_include/local-vdk_safe-0.1.0/dist/vdk_safe.js';


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
 * Block until lambda returns true or timeout
 *
 * `timeout_ms` should be divisible by `interval_ms`
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
     interval_ms : number)
    : Promise<vdk_safe.Safe<ok_t, string>>
{
    let max_iters : number = Math.floor(timeout_ms / interval_ms);

    for (let i  = 1;
             i <= max_iters;
             i++)
    {
        if (fun()) {
            let result = vdk_safe.ok(result_fun());
            return result;
        }
        else
            await sleep(interval_ms);
    }

    let result = vdk_safe.error(timeout_msg);
    return result;
}
