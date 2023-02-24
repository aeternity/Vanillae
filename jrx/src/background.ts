/**
 * JR Controller
 *
 * Note that this is a *thread*.  We put our business and storage logic within
 * this thread.  The reason we do that is we don't want to deal with the
 * problem of conflicting concurrent writes..
 *
 * @module
 */

export {
    // TYPES
    jr_data,
    named_keypair,
    keypair,
    // CONSTANTS
    jr_data_mzero
};



//=============================================================================
// TYPES
//=============================================================================

/**
 * The data structure we store
 */
type jr_data
    = {version        : 0,
       named_keypairs : Array<named_keypair>};



/**
 * Every keypair has to have a name
 */
type named_keypair
    = {name : string}
      & keypair;



/**
 * From NaCl
 */
type keypair
    = {secretKey : Uint8Array,
       publicKey : Uint8Array};



//=============================================================================
// CONSTANTS
//=============================================================================

/**
 * This is the default {@link jr_data}.
 *
 * @private
 */
let jr_data_mzero = {version        : 0,
                     named_keypairs : []};




//=============================================================================
// INTERFACE FUNCTIONS
//=============================================================================




//=============================================================================
// INTERNALS
//=============================================================================

main();

/**
 * The main function
 */
function
main
    ()
    : void
{
}
