let detect_msg
    = {id        : "urmom",
       name      : "JR",
       networkId : "ae_uat",
       origin    : "ur dads balls",
       type      : "extension"};

let detect_awcp_msg
    = {type : "to_aepp",
       data : {jsonrpc : "2.0",
               method  : "connection.announcePresence",
               params  : detect_msg}};



function post_detect_msg() {
    window.postMessage(detect_awcp_msg, '*');
}


async function mk_detectable() {
    //while (true) {
    // 3 seconds times 40 is 2 minutes
    for (let i=1; i<=40; i++) {
        console.error('pee');
        post_detect_msg();
        await sleep(3000);
    }
}

function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


/**
 * This handles messages from *other parts of the extension*
 */
function handler(msg: any) {
    console.error('message: ', msg);
    switch(msg) {
        case 'mk-detectable':
            mk_detectable();
            return;
        default:
            console.error('your mom is dead');
    }
}

/**
 * This handles messages from page scripts
 */
function window_message_handler(msg: {data: any}) {
    console.error('the science is coming', msg);
    // example science:
    // {
    //   "type": "to_waellet",
    //   "data": {
    //     "jsonrpc": "2.0",
    //     "id": "ske-connect-1",
    //     "method": "connection.open",
    //     "params": {
    //       "name": "sidekick examples",
    //       "version": 1
    //     }
    //   }
    // }
    let the_science = msg.data;
    console.error('THE SCIENCE: ', the_science);
    console.error('is the science good or bad?');
    if (the_science.type === 'to_waellet') {
        console.error('the science is good');
        the_science_is_good(the_science.data);
    }
    else {
        console.error('the science is bad');
    }
}

/**
 * window_message_handler handles all messages sent into the event bus,
 * including messages that we sent (yay js). window_message_handler branches on
 * whether the message is for us or not (is the science good or bad?). If the
 * message is for us (the science is good), then this function is triggered.
 *
 * The bottom line is this is the function that *actually* handles messages
 * from the window
 */
function the_science_is_good(awcp_rcp_data: {id: string|number, method: string, params: object}) {
    // example science data:
    // // layer 3: json rpc
    // {jsonrpc : "2.0",
    //  id      : "ske-connect-1",
    //  method  : "connection.open",
    //            // layer 4: AWCP-specific semantics
    //  params  : {name    : "sidekick examples",
    //             version : 1}}}
    // can assume it is a call
    let msg_ident  = awcp_rcp_data.id;
    let msg_method = awcp_rcp_data.method;
    // branch here on the "method" field
    switch (msg_method) {
        case "connection.open":
            post_connect_msg(msg_ident);
            break;
        case "address.subscribe":
            bg_address_subscribe(msg_ident);
            break;
        case "transaction.sign":
            bg_tx_sign(msg_ident, awcp_rcp_data.params);
            break;
        case "message.sign":
            bg_msg_sign(msg_ident, awcp_rcp_data.params);
            break;
        default:
            console.error('the science is worse than i initially thought');
            console.error("we're going to have to put the science to sleep");
    }
}

/**
 * Called in response to "connection.open" requests from page scripts
 *
 * FIXME: this should query the user to see if he wants to connect
 */
function post_connect_msg(msg_ident: string|number) {
    // : EventData_W2A_connection_open
    // http://localhost:6969/local-awcp-0.2.1/types/EventData_W2A_connection_open.html
    let connect_response =
            {type: "to_aepp",
             data: {jsonrpc: "2.0",
                    id: msg_ident,
                    method: "connection.open",
                    result: detect_msg}};
    window.postMessage(connect_response, '*');
}


/**
 * Called in response to "address.subscribe" requests from page scripts
 *
 * this is to get the wallet's address
 */
function bg_address_subscribe(msg_ident: string|number) {
}


/**
 * Called in response to "transaction.sign" requests from page scripts
 *
 * user wants us to sign a transaction
 */
function bg_tx_sign(msg_ident: string|number, params: object) {
}

/**
 * Called in response to "message.sign" requests from page scripts
 *
 * user wants us to sign a message
 */
function bg_msg_sign(msg_ident: string|number, params: object) {
}

// @ts-ignore browser
browser.runtime.onMessage.addListener(handler);
window.addEventListener('message', window_message_handler);
//window.addEventListener('message', function(evt) { console.error('BYYYYYYYYYYYYYYY', evt) });
//window.addEventListener('click', function() { console.error('HERRRO'); });

console.error('poop');
