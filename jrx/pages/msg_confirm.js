console.log('ur mom');
var result = null;
document.getElementById('good').onclick = () => { console.log('click good'); result = 'good' }
document.getElementById('bad').onclick = () => { console.log('click bad'); result = 'bad' }

// for some reason the listener here never gets triggered
async function listener(msg, _sender, _sendResponse) {
    console.log('listener triggered');
    console.log(msg);
}

console.log('adding listener');
browser.runtime.onMessage.addListener(listener);
console.log('added listener');
