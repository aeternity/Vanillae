async function active_tab_id() {
    let active_tabs = await browser.tabs.query({active: true, currentWindow: true});
    return active_tabs[0].id;
}

async function mk_detectable() {
    let ati = await active_tab_id();
    browser.tabs.sendMessage(ati,               // active tab
                             'mk-detectable');  // message
    document.getElementById('mk-detectable').disabled = true;

}

// detect button action
document.getElementById('mk-detectable').onclick = mk_detectable;
