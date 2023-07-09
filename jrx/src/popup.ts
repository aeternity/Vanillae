/**
 * This is the script for the popup window
 */

main();

/**
 * runs whenever the popup opens
 */
async function
main
    ()
    : Promise<void>
{
    // detect button action
    document.getElementById('generate')!.onclick = generate_keypair;
}


async function
generate_keypair
    ()
{
    let keypair = nacl.sign.keyPair();
    document.getElementById('scratch')!.innerHTML += JSON.stringify(keypair, undefined, 4);
}
