// see b_lib.ts, function gb for the thing that talks to this
main();

async function main() {
    // connect to the runtime
    let port = browser.runtime.connect();
    // wait for a message
    port.onMessage.addListener(
        function({title, miscinfo, timeout_ms}) {
            console.log('title', title);
            console.log('miscinfo', miscinfo);
            console.log('timeout_ms', timeout_ms);
            document.getElementById('title-h1').innerHTML = title;
            document.getElementById('miscinfo').innerHTML = miscinfo;
        }
    );
}
