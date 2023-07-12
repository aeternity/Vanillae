/**
 * Definitions of messages that are passed between process A and process B in
 * JR. Example: how the content script talks to the background script.
 *
 * @module
 */

/**
 * this is the message sent from the content script to the background script
 * when a page script requests
 */
type c2b_addr_subscribe = "address_subscribe";
