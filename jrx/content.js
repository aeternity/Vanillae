let detect_msg =
        {id        : "urmom",
         name      : "JR",
         networkId : "ae_uat",
         origin    : "ur dads balls",
         type      : "extension"};

let detect_awcp_msg =
        {type: "to_aepp",
         data: {jsonrpc: "2.0",
                method: "connection.announcePresence",
                params: detect_msg}};



function post_detect_msg() {
    window.postMessage(detect_awcp_msg, '*');
}

function post_connect_msg(msg_ident) {
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

async function mk_detectable() {
    while (true) {
        console.error('pee');
        post_detect_msg();
        await sleep(3000);
    }
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


function handler(msg) {
    console.error('message: ', msg);
    switch(msg) {
        case 'mk-detectable':
            mk_detectable();
            return;
        default:
            console.error('your mom is dead');
    }
}

function window_message_handler(msg) {
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

function the_science_is_good(awcp_rcp_data) {
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
    switch (msg_method) {
        case "connection.open":
            post_connect_msg(msg_ident);
            return;
        default:
            console.error('the science is worse than i initially thought');
            console.error("we're going to have to put the science to sleep");
    }
}

browser.runtime.onMessage.addListener(handler);
window.addEventListener('message', window_message_handler);
//window.addEventListener('message', function(evt) { console.error('BYYYYYYYYYYYYYYY', evt) });
window.addEventListener('click', function() { console.error('HERRRO'); });

console.error('poop');
