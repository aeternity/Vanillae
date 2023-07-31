/**
 * This is the "background script", which just calls b_lib:main/0
 *
 * b_lib is the background "library". I want the popup and content script to be
 * able to "call" the background script (really there's a rewrite layer there
 * so the entire IPC messaging protocol is in one file). I don't want to fire the
 * missiles whenever say the popup script loads the "background library", so
 * the background "script" is a separate file.
 */

import {b_main} from './b_lib.js';

b_main();
