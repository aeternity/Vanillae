console.log('ur mom');
var result = null;
document.getElementById('good').onclick = () => { console.log('click good'); result = 'good' }
document.getElementById('bad').onclick = () => { console.log('click bad'); result = 'bad' }

browser.runtime.onMessage.addListener((msg) => {
    console.error("ERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRROR");
    console.log('msg', msg);
    //document.getElementById("message").innerHTML = msg_str;

    //const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

    //while (true) {
    //    if (result === 'good') {
    //        console.log('good');
    //        return true;
    //    }
    //    else if (result === 'bad') {
    //        console.log('bad');
    //        return false;
    //    }
    //    else
    //        await sleep(100);
    //}
});
