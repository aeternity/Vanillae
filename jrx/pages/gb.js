// see b_lib.ts, function gb for the thing that talks to this
main();

async function main() {
    // connect to the runtime
    let port = browser.runtime.connect();
    // wait for a message
    port.onMessage.addListener(
        function({title, miscinfo}) {
            console.log('title', title);
            console.log('miscinfo', miscinfo);
            document.getElementById('title-h1').innerHTML = title;
            document.getElementById('miscinfo').innerHTML = miscinfo;

            document.getElementById('good').onclick = function() { port.postMessage('good'); };
            document.getElementById('bad').onclick = function() { port.postMessage('bad'); };
        }
    );
}
