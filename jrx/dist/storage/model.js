"use strict";
/* idea: versioned state
 *
 * there is a version 0 state
 *
 * each state migration contains a migration from the previous version to the
 * new version
 *
 * laws:
 *
 * - all version N states must map to valid version N+1 states
 *
 */
//export {
//    jr_version,
//    jr_state
//};
//
//let jr_version = 0;
//
///**
// * current state type for current version
// */
//type jr_state = jr_0_state;
//
///**
// * All jr state types, historically
// */
//type jr_n_state
//    = jr_0_state;
//
///**
// * This is the version 0 state
// */
//type jr_0_state
//    = {version : 0,
//       keypair : keypair};
//
//type keypair
//    = {secretKey: Uint8Array
//# sourceMappingURL=model.js.map