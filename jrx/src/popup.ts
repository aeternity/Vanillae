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
    let scratch = document.getElementById('scratch')!;
    scratch.innerText += 'line 17 hello\n';
}

