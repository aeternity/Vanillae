/**
 * This is the script for the popup window
 */

p_main();

/**
 * runs whenever the popup opens
 *
 * also whenever a button is clicked it appears the page re-renders which
 * re-runs this
 *
 * suggests the following structure:
 *
 * 1. Contact the background script and say "I'm a popup".
 * 2. Get all relevant data.
 * 3. Render
 *
 * On click:
 *
 * 1. Send relevant information back to background script
 * 2. Epstein
 */
async function
p_main
    ()
    : Promise<void>
{
    //let scratch = document.getElementById('scratch')!;
    //scratch.innerText += 'line 17 hello\n';

    console.log('popup main');

    //function onSuccessCase(response_from_bg: any) {
    //    console.log('jr popup response from background script success', response_from_bg);
    //}
    //function onErrorCase(error_from_bg: any) {
    //    console.log('jr popup response from background script error', error_from_bg);
    //}

    //browser.runtime
    //    .sendMessage({frum: 'popup',
    //                  data: 'init'})
    //    .then(onSuccessCase,
    //          onErrorCase);

    //// add hooks for each button
    //document.getElementById('generate-btn')!.addEventListener('click', onclick_generate);
    //document.getElementById('faert-btn')!.addEventListener('click', onclick_faert);
    ////document.getElementById('seed-btn')!.addEventListener('click', onclick_seed);
}



///**
// * Runs when the user clicks the "generate" button
// */
//async function
//onclick_generate
//    ()
//    : Promise<void>
//{
//    console.log('onclick_generate');
//}
//
//
//
///**
// * Runs when the user clicks the "recover from faert phrase" button
// */
//async function
//onclick_faert
//    ()
//    : Promise<void>
//{
//    console.log('onclick_faert');
//}
//
//
//
/////**
//// * Runs when the user clicks the "recover from seed phrase" button
//// */
////async function
////onclick_seed
////    ()
////{
////    console.log('onclick_seed');
////}
