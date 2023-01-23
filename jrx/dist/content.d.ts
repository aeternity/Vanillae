declare let detect_msg: {
    id: string;
    name: string;
    networkId: string;
    origin: string;
    type: string;
};
declare let detect_awcp_msg: {
    type: string;
    data: {
        jsonrpc: string;
        method: string;
        params: {
            id: string;
            name: string;
            networkId: string;
            origin: string;
            type: string;
        };
    };
};
declare function post_detect_msg(): void;
declare function mk_detectable(): Promise<void>;
declare function sleep(ms: number): Promise<unknown>;
/**
 * This handles messages from *other parts of the extension*
 */
declare function handler(msg: any): void;
/**
 * This handles messages from page scripts
 */
declare function window_message_handler(msg: {
    data: any;
}): void;
/**
 * window_message_handler handles all messages sent into the event bus,
 * including messages that we sent (yay js). window_message_handler branches on
 * whether the message is for us or not (is the science good or bad?). If the
 * message is for us (the science is good), then this function is triggered.
 *
 * The bottom line is this is the function that *actually* handles messages
 * from the window
 */
declare function the_science_is_good(awcp_rcp_data: {
    id: string | number;
    method: string;
    params: object;
}): void;
/**
 * Called in response to "connection.open" requests from page scripts
 *
 * FIXME: this should query the user to see if he wants to connect
 */
declare function post_connect_msg(msg_ident: string | number): void;
/**
 * Called in response to "address.subscribe" requests from page scripts
 *
 * this is to get the wallet's address
 */
declare function bg_address_subscribe(msg_ident: string | number): void;
/**
 * Called in response to "transaction.sign" requests from page scripts
 *
 * user wants us to sign a transaction
 */
declare function bg_tx_sign(msg_ident: string | number, params: object): void;
/**
 * Called in response to "message.sign" requests from page scripts
 *
 * user wants us to sign a message
 */
declare function bg_msg_sign(msg_ident: string | number, params: object): void;
