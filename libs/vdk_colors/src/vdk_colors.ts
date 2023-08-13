/**
 * Miscellaneous helper functions for RGB/HSV color arithmetic
 *
 * Reference: https://en.wikipedia.org/wiki/HSL_and_HSV
 *
 * @module
 */

export {
};

export type {
    rgb
}



/**
 * standard red-green-blue color representation
 *
 * attributes `r`, `g` and `b` each are integers ranging within `0..255` inclusive
 */
type rgb
    = {r : number,
       g : number,
       b : number};



/**
 * hue-saturation-value color representation
 *
 - * `h` is an inte
 */
type hsv
    = {h : number,
       s : number,
       v : number};
