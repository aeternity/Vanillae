(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
// Blake2B in pure Javascript
// Adapted from the reference implementation in RFC7693
// Ported to Javascript by DC - https://github.com/dcposch

const util = require('./util')

// 64-bit unsigned addition
// Sets v[a,a+1] += v[b,b+1]
// v should be a Uint32Array
function ADD64AA (v, a, b) {
  const o0 = v[a] + v[b]
  let o1 = v[a + 1] + v[b + 1]
  if (o0 >= 0x100000000) {
    o1++
  }
  v[a] = o0
  v[a + 1] = o1
}

// 64-bit unsigned addition
// Sets v[a,a+1] += b
// b0 is the low 32 bits of b, b1 represents the high 32 bits
function ADD64AC (v, a, b0, b1) {
  let o0 = v[a] + b0
  if (b0 < 0) {
    o0 += 0x100000000
  }
  let o1 = v[a + 1] + b1
  if (o0 >= 0x100000000) {
    o1++
  }
  v[a] = o0
  v[a + 1] = o1
}

// Little-endian byte access
function B2B_GET32 (arr, i) {
  return arr[i] ^ (arr[i + 1] << 8) ^ (arr[i + 2] << 16) ^ (arr[i + 3] << 24)
}

// G Mixing function
// The ROTRs are inlined for speed
function B2B_G (a, b, c, d, ix, iy) {
  const x0 = m[ix]
  const x1 = m[ix + 1]
  const y0 = m[iy]
  const y1 = m[iy + 1]

  ADD64AA(v, a, b) // v[a,a+1] += v[b,b+1] ... in JS we must store a uint64 as two uint32s
  ADD64AC(v, a, x0, x1) // v[a, a+1] += x ... x0 is the low 32 bits of x, x1 is the high 32 bits

  // v[d,d+1] = (v[d,d+1] xor v[a,a+1]) rotated to the right by 32 bits
  let xor0 = v[d] ^ v[a]
  let xor1 = v[d + 1] ^ v[a + 1]
  v[d] = xor1
  v[d + 1] = xor0

  ADD64AA(v, c, d)

  // v[b,b+1] = (v[b,b+1] xor v[c,c+1]) rotated right by 24 bits
  xor0 = v[b] ^ v[c]
  xor1 = v[b + 1] ^ v[c + 1]
  v[b] = (xor0 >>> 24) ^ (xor1 << 8)
  v[b + 1] = (xor1 >>> 24) ^ (xor0 << 8)

  ADD64AA(v, a, b)
  ADD64AC(v, a, y0, y1)

  // v[d,d+1] = (v[d,d+1] xor v[a,a+1]) rotated right by 16 bits
  xor0 = v[d] ^ v[a]
  xor1 = v[d + 1] ^ v[a + 1]
  v[d] = (xor0 >>> 16) ^ (xor1 << 16)
  v[d + 1] = (xor1 >>> 16) ^ (xor0 << 16)

  ADD64AA(v, c, d)

  // v[b,b+1] = (v[b,b+1] xor v[c,c+1]) rotated right by 63 bits
  xor0 = v[b] ^ v[c]
  xor1 = v[b + 1] ^ v[c + 1]
  v[b] = (xor1 >>> 31) ^ (xor0 << 1)
  v[b + 1] = (xor0 >>> 31) ^ (xor1 << 1)
}

// Initialization Vector
const BLAKE2B_IV32 = new Uint32Array([
  0xf3bcc908, 0x6a09e667, 0x84caa73b, 0xbb67ae85, 0xfe94f82b, 0x3c6ef372,
  0x5f1d36f1, 0xa54ff53a, 0xade682d1, 0x510e527f, 0x2b3e6c1f, 0x9b05688c,
  0xfb41bd6b, 0x1f83d9ab, 0x137e2179, 0x5be0cd19
])

const SIGMA8 = [
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4, 8, 9, 15, 13,
  6, 1, 12, 0, 2, 11, 7, 5, 3, 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1,
  9, 4, 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8, 9, 0, 5, 7, 2, 4,
  10, 15, 14, 1, 11, 12, 6, 8, 3, 13, 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5,
  15, 14, 1, 9, 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11, 13, 11, 7,
  14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10, 6, 15, 14, 9, 11, 3, 0, 8, 12, 2,
  13, 7, 1, 4, 10, 5, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0, 0,
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 10, 4, 8, 9, 15, 13, 6,
  1, 12, 0, 2, 11, 7, 5, 3
]

// These are offsets into a uint64 buffer.
// Multiply them all by 2 to make them offsets into a uint32 buffer,
// because this is Javascript and we don't have uint64s
const SIGMA82 = new Uint8Array(
  SIGMA8.map(function (x) {
    return x * 2
  })
)

// Compression function. 'last' flag indicates last block.
// Note we're representing 16 uint64s as 32 uint32s
const v = new Uint32Array(32)
const m = new Uint32Array(32)
function blake2bCompress (ctx, last) {
  let i = 0

  // init work variables
  for (i = 0; i < 16; i++) {
    v[i] = ctx.h[i]
    v[i + 16] = BLAKE2B_IV32[i]
  }

  // low 64 bits of offset
  v[24] = v[24] ^ ctx.t
  v[25] = v[25] ^ (ctx.t / 0x100000000)
  // high 64 bits not supported, offset may not be higher than 2**53-1

  // last block flag set ?
  if (last) {
    v[28] = ~v[28]
    v[29] = ~v[29]
  }

  // get little-endian words
  for (i = 0; i < 32; i++) {
    m[i] = B2B_GET32(ctx.b, 4 * i)
  }

  // twelve rounds of mixing
  // uncomment the DebugPrint calls to log the computation
  // and match the RFC sample documentation
  // util.debugPrint('          m[16]', m, 64)
  for (i = 0; i < 12; i++) {
    // util.debugPrint('   (i=' + (i < 10 ? ' ' : '') + i + ') v[16]', v, 64)
    B2B_G(0, 8, 16, 24, SIGMA82[i * 16 + 0], SIGMA82[i * 16 + 1])
    B2B_G(2, 10, 18, 26, SIGMA82[i * 16 + 2], SIGMA82[i * 16 + 3])
    B2B_G(4, 12, 20, 28, SIGMA82[i * 16 + 4], SIGMA82[i * 16 + 5])
    B2B_G(6, 14, 22, 30, SIGMA82[i * 16 + 6], SIGMA82[i * 16 + 7])
    B2B_G(0, 10, 20, 30, SIGMA82[i * 16 + 8], SIGMA82[i * 16 + 9])
    B2B_G(2, 12, 22, 24, SIGMA82[i * 16 + 10], SIGMA82[i * 16 + 11])
    B2B_G(4, 14, 16, 26, SIGMA82[i * 16 + 12], SIGMA82[i * 16 + 13])
    B2B_G(6, 8, 18, 28, SIGMA82[i * 16 + 14], SIGMA82[i * 16 + 15])
  }
  // util.debugPrint('   (i=12) v[16]', v, 64)

  for (i = 0; i < 16; i++) {
    ctx.h[i] = ctx.h[i] ^ v[i] ^ v[i + 16]
  }
  // util.debugPrint('h[8]', ctx.h, 64)
}

// reusable parameterBlock
const parameterBlock = new Uint8Array([
  0,
  0,
  0,
  0, //  0: outlen, keylen, fanout, depth
  0,
  0,
  0,
  0, //  4: leaf length, sequential mode
  0,
  0,
  0,
  0, //  8: node offset
  0,
  0,
  0,
  0, // 12: node offset
  0,
  0,
  0,
  0, // 16: node depth, inner length, rfu
  0,
  0,
  0,
  0, // 20: rfu
  0,
  0,
  0,
  0, // 24: rfu
  0,
  0,
  0,
  0, // 28: rfu
  0,
  0,
  0,
  0, // 32: salt
  0,
  0,
  0,
  0, // 36: salt
  0,
  0,
  0,
  0, // 40: salt
  0,
  0,
  0,
  0, // 44: salt
  0,
  0,
  0,
  0, // 48: personal
  0,
  0,
  0,
  0, // 52: personal
  0,
  0,
  0,
  0, // 56: personal
  0,
  0,
  0,
  0 // 60: personal
])

// Creates a BLAKE2b hashing context
// Requires an output length between 1 and 64 bytes
// Takes an optional Uint8Array key
// Takes an optinal Uint8Array salt
// Takes an optinal Uint8Array personal
function blake2bInit (outlen, key, salt, personal) {
  if (outlen === 0 || outlen > 64) {
    throw new Error('Illegal output length, expected 0 < length <= 64')
  }
  if (key && key.length > 64) {
    throw new Error('Illegal key, expected Uint8Array with 0 < length <= 64')
  }
  if (salt && salt.length !== 16) {
    throw new Error('Illegal salt, expected Uint8Array with length is 16')
  }
  if (personal && personal.length !== 16) {
    throw new Error('Illegal personal, expected Uint8Array with length is 16')
  }

  // state, 'param block'
  const ctx = {
    b: new Uint8Array(128),
    h: new Uint32Array(16),
    t: 0, // input count
    c: 0, // pointer within buffer
    outlen: outlen // output length in bytes
  }

  // initialize parameterBlock before usage
  parameterBlock.fill(0)
  parameterBlock[0] = outlen
  if (key) parameterBlock[1] = key.length
  parameterBlock[2] = 1 // fanout
  parameterBlock[3] = 1 // depth
  if (salt) parameterBlock.set(salt, 32)
  if (personal) parameterBlock.set(personal, 48)

  // initialize hash state
  for (let i = 0; i < 16; i++) {
    ctx.h[i] = BLAKE2B_IV32[i] ^ B2B_GET32(parameterBlock, i * 4)
  }

  // key the hash, if applicable
  if (key) {
    blake2bUpdate(ctx, key)
    // at the end
    ctx.c = 128
  }

  return ctx
}

// Updates a BLAKE2b streaming hash
// Requires hash context and Uint8Array (byte array)
function blake2bUpdate (ctx, input) {
  for (let i = 0; i < input.length; i++) {
    if (ctx.c === 128) {
      // buffer full ?
      ctx.t += ctx.c // add counters
      blake2bCompress(ctx, false) // compress (not last)
      ctx.c = 0 // counter to zero
    }
    ctx.b[ctx.c++] = input[i]
  }
}

// Completes a BLAKE2b streaming hash
// Returns a Uint8Array containing the message digest
function blake2bFinal (ctx) {
  ctx.t += ctx.c // mark last block offset

  while (ctx.c < 128) {
    // fill up with zeros
    ctx.b[ctx.c++] = 0
  }
  blake2bCompress(ctx, true) // final block flag = 1

  // little endian convert and store
  const out = new Uint8Array(ctx.outlen)
  for (let i = 0; i < ctx.outlen; i++) {
    out[i] = ctx.h[i >> 2] >> (8 * (i & 3))
  }
  return out
}

// Computes the BLAKE2B hash of a string or byte array, and returns a Uint8Array
//
// Returns a n-byte Uint8Array
//
// Parameters:
// - input - the input bytes, as a string, Buffer or Uint8Array
// - key - optional key Uint8Array, up to 64 bytes
// - outlen - optional output length in bytes, default 64
// - salt - optional salt bytes, string, Buffer or Uint8Array
// - personal - optional personal bytes, string, Buffer or Uint8Array
function blake2b (input, key, outlen, salt, personal) {
  // preprocess inputs
  outlen = outlen || 64
  input = util.normalizeInput(input)
  if (salt) {
    salt = util.normalizeInput(salt)
  }
  if (personal) {
    personal = util.normalizeInput(personal)
  }

  // do the math
  const ctx = blake2bInit(outlen, key, salt, personal)
  blake2bUpdate(ctx, input)
  return blake2bFinal(ctx)
}

// Computes the BLAKE2B hash of a string or byte array
//
// Returns an n-byte hash in hex, all lowercase
//
// Parameters:
// - input - the input bytes, as a string, Buffer, or Uint8Array
// - key - optional key Uint8Array, up to 64 bytes
// - outlen - optional output length in bytes, default 64
// - salt - optional salt bytes, string, Buffer or Uint8Array
// - personal - optional personal bytes, string, Buffer or Uint8Array
function blake2bHex (input, key, outlen, salt, personal) {
  const output = blake2b(input, key, outlen, salt, personal)
  return util.toHex(output)
}

module.exports = {
  blake2b: blake2b,
  blake2bHex: blake2bHex,
  blake2bInit: blake2bInit,
  blake2bUpdate: blake2bUpdate,
  blake2bFinal: blake2bFinal
}

},{"./util":4}],2:[function(require,module,exports){
// BLAKE2s hash function in pure Javascript
// Adapted from the reference implementation in RFC7693
// Ported to Javascript by DC - https://github.com/dcposch

const util = require('./util')

// Little-endian byte access.
// Expects a Uint8Array and an index
// Returns the little-endian uint32 at v[i..i+3]
function B2S_GET32 (v, i) {
  return v[i] ^ (v[i + 1] << 8) ^ (v[i + 2] << 16) ^ (v[i + 3] << 24)
}

// Mixing function G.
function B2S_G (a, b, c, d, x, y) {
  v[a] = v[a] + v[b] + x
  v[d] = ROTR32(v[d] ^ v[a], 16)
  v[c] = v[c] + v[d]
  v[b] = ROTR32(v[b] ^ v[c], 12)
  v[a] = v[a] + v[b] + y
  v[d] = ROTR32(v[d] ^ v[a], 8)
  v[c] = v[c] + v[d]
  v[b] = ROTR32(v[b] ^ v[c], 7)
}

// 32-bit right rotation
// x should be a uint32
// y must be between 1 and 31, inclusive
function ROTR32 (x, y) {
  return (x >>> y) ^ (x << (32 - y))
}

// Initialization Vector.
const BLAKE2S_IV = new Uint32Array([
  0x6a09e667,
  0xbb67ae85,
  0x3c6ef372,
  0xa54ff53a,
  0x510e527f,
  0x9b05688c,
  0x1f83d9ab,
  0x5be0cd19
])

const SIGMA = new Uint8Array([
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12,
  13,
  14,
  15,
  14,
  10,
  4,
  8,
  9,
  15,
  13,
  6,
  1,
  12,
  0,
  2,
  11,
  7,
  5,
  3,
  11,
  8,
  12,
  0,
  5,
  2,
  15,
  13,
  10,
  14,
  3,
  6,
  7,
  1,
  9,
  4,
  7,
  9,
  3,
  1,
  13,
  12,
  11,
  14,
  2,
  6,
  5,
  10,
  4,
  0,
  15,
  8,
  9,
  0,
  5,
  7,
  2,
  4,
  10,
  15,
  14,
  1,
  11,
  12,
  6,
  8,
  3,
  13,
  2,
  12,
  6,
  10,
  0,
  11,
  8,
  3,
  4,
  13,
  7,
  5,
  15,
  14,
  1,
  9,
  12,
  5,
  1,
  15,
  14,
  13,
  4,
  10,
  0,
  7,
  6,
  3,
  9,
  2,
  8,
  11,
  13,
  11,
  7,
  14,
  12,
  1,
  3,
  9,
  5,
  0,
  15,
  4,
  8,
  6,
  2,
  10,
  6,
  15,
  14,
  9,
  11,
  3,
  0,
  8,
  12,
  2,
  13,
  7,
  1,
  4,
  10,
  5,
  10,
  2,
  8,
  4,
  7,
  6,
  1,
  5,
  15,
  11,
  9,
  14,
  3,
  12,
  13,
  0
])

// Compression function. "last" flag indicates last block
const v = new Uint32Array(16)
const m = new Uint32Array(16)
function blake2sCompress (ctx, last) {
  let i = 0
  for (i = 0; i < 8; i++) {
    // init work variables
    v[i] = ctx.h[i]
    v[i + 8] = BLAKE2S_IV[i]
  }

  v[12] ^= ctx.t // low 32 bits of offset
  v[13] ^= ctx.t / 0x100000000 // high 32 bits
  if (last) {
    // last block flag set ?
    v[14] = ~v[14]
  }

  for (i = 0; i < 16; i++) {
    // get little-endian words
    m[i] = B2S_GET32(ctx.b, 4 * i)
  }

  // ten rounds of mixing
  // uncomment the DebugPrint calls to log the computation
  // and match the RFC sample documentation
  // util.debugPrint('          m[16]', m, 32)
  for (i = 0; i < 10; i++) {
    // util.debugPrint('   (i=' + i + ')  v[16]', v, 32)
    B2S_G(0, 4, 8, 12, m[SIGMA[i * 16 + 0]], m[SIGMA[i * 16 + 1]])
    B2S_G(1, 5, 9, 13, m[SIGMA[i * 16 + 2]], m[SIGMA[i * 16 + 3]])
    B2S_G(2, 6, 10, 14, m[SIGMA[i * 16 + 4]], m[SIGMA[i * 16 + 5]])
    B2S_G(3, 7, 11, 15, m[SIGMA[i * 16 + 6]], m[SIGMA[i * 16 + 7]])
    B2S_G(0, 5, 10, 15, m[SIGMA[i * 16 + 8]], m[SIGMA[i * 16 + 9]])
    B2S_G(1, 6, 11, 12, m[SIGMA[i * 16 + 10]], m[SIGMA[i * 16 + 11]])
    B2S_G(2, 7, 8, 13, m[SIGMA[i * 16 + 12]], m[SIGMA[i * 16 + 13]])
    B2S_G(3, 4, 9, 14, m[SIGMA[i * 16 + 14]], m[SIGMA[i * 16 + 15]])
  }
  // util.debugPrint('   (i=10) v[16]', v, 32)

  for (i = 0; i < 8; i++) {
    ctx.h[i] ^= v[i] ^ v[i + 8]
  }
  // util.debugPrint('h[8]', ctx.h, 32)
}

// Creates a BLAKE2s hashing context
// Requires an output length between 1 and 32 bytes
// Takes an optional Uint8Array key
function blake2sInit (outlen, key) {
  if (!(outlen > 0 && outlen <= 32)) {
    throw new Error('Incorrect output length, should be in [1, 32]')
  }
  const keylen = key ? key.length : 0
  if (key && !(keylen > 0 && keylen <= 32)) {
    throw new Error('Incorrect key length, should be in [1, 32]')
  }

  const ctx = {
    h: new Uint32Array(BLAKE2S_IV), // hash state
    b: new Uint8Array(64), // input block
    c: 0, // pointer within block
    t: 0, // input count
    outlen: outlen // output length in bytes
  }
  ctx.h[0] ^= 0x01010000 ^ (keylen << 8) ^ outlen

  if (keylen > 0) {
    blake2sUpdate(ctx, key)
    ctx.c = 64 // at the end
  }

  return ctx
}

// Updates a BLAKE2s streaming hash
// Requires hash context and Uint8Array (byte array)
function blake2sUpdate (ctx, input) {
  for (let i = 0; i < input.length; i++) {
    if (ctx.c === 64) {
      // buffer full ?
      ctx.t += ctx.c // add counters
      blake2sCompress(ctx, false) // compress (not last)
      ctx.c = 0 // counter to zero
    }
    ctx.b[ctx.c++] = input[i]
  }
}

// Completes a BLAKE2s streaming hash
// Returns a Uint8Array containing the message digest
function blake2sFinal (ctx) {
  ctx.t += ctx.c // mark last block offset
  while (ctx.c < 64) {
    // fill up with zeros
    ctx.b[ctx.c++] = 0
  }
  blake2sCompress(ctx, true) // final block flag = 1

  // little endian convert and store
  const out = new Uint8Array(ctx.outlen)
  for (let i = 0; i < ctx.outlen; i++) {
    out[i] = (ctx.h[i >> 2] >> (8 * (i & 3))) & 0xff
  }
  return out
}

// Computes the BLAKE2S hash of a string or byte array, and returns a Uint8Array
//
// Returns a n-byte Uint8Array
//
// Parameters:
// - input - the input bytes, as a string, Buffer, or Uint8Array
// - key - optional key Uint8Array, up to 32 bytes
// - outlen - optional output length in bytes, default 64
function blake2s (input, key, outlen) {
  // preprocess inputs
  outlen = outlen || 32
  input = util.normalizeInput(input)

  // do the math
  const ctx = blake2sInit(outlen, key)
  blake2sUpdate(ctx, input)
  return blake2sFinal(ctx)
}

// Computes the BLAKE2S hash of a string or byte array
//
// Returns an n-byte hash in hex, all lowercase
//
// Parameters:
// - input - the input bytes, as a string, Buffer, or Uint8Array
// - key - optional key Uint8Array, up to 32 bytes
// - outlen - optional output length in bytes, default 64
function blake2sHex (input, key, outlen) {
  const output = blake2s(input, key, outlen)
  return util.toHex(output)
}

module.exports = {
  blake2s: blake2s,
  blake2sHex: blake2sHex,
  blake2sInit: blake2sInit,
  blake2sUpdate: blake2sUpdate,
  blake2sFinal: blake2sFinal
}

},{"./util":4}],3:[function(require,module,exports){
const b2b = require('./blake2b')
const b2s = require('./blake2s')

module.exports = {
  blake2b: b2b.blake2b,
  blake2bHex: b2b.blake2bHex,
  blake2bInit: b2b.blake2bInit,
  blake2bUpdate: b2b.blake2bUpdate,
  blake2bFinal: b2b.blake2bFinal,
  blake2s: b2s.blake2s,
  blake2sHex: b2s.blake2sHex,
  blake2sInit: b2s.blake2sInit,
  blake2sUpdate: b2s.blake2sUpdate,
  blake2sFinal: b2s.blake2sFinal
}

},{"./blake2b":1,"./blake2s":2}],4:[function(require,module,exports){
const ERROR_MSG_INPUT = 'Input must be an string, Buffer or Uint8Array'

// For convenience, let people hash a string, not just a Uint8Array
function normalizeInput (input) {
  let ret
  if (input instanceof Uint8Array) {
    ret = input
  } else if (typeof input === 'string') {
    const encoder = new TextEncoder()
    ret = encoder.encode(input)
  } else {
    throw new Error(ERROR_MSG_INPUT)
  }
  return ret
}

// Converts a Uint8Array to a hexadecimal string
// For example, toHex([255, 0, 255]) returns "ff00ff"
function toHex (bytes) {
  return Array.prototype.map
    .call(bytes, function (n) {
      return (n < 16 ? '0' : '') + n.toString(16)
    })
    .join('')
}

// Converts any value in [0...2^32-1] to an 8-character hex string
function uint32ToHex (val) {
  return (0x100000000 + val).toString(16).substring(1)
}

// For debugging: prints out hash state in the same format as the RFC
// sample computation exactly, so that you can diff
function debugPrint (label, arr, size) {
  let msg = '\n' + label + ' = '
  for (let i = 0; i < arr.length; i += 2) {
    if (size === 32) {
      msg += uint32ToHex(arr[i]).toUpperCase()
      msg += ' '
      msg += uint32ToHex(arr[i + 1]).toUpperCase()
    } else if (size === 64) {
      msg += uint32ToHex(arr[i + 1]).toUpperCase()
      msg += uint32ToHex(arr[i]).toUpperCase()
    } else throw new Error('Invalid size ' + size)
    if (i % 6 === 4) {
      msg += '\n' + new Array(label.length + 4).join(' ')
    } else if (i < arr.length - 2) {
      msg += ' '
    }
  }
  console.log(msg)
}

// For performance testing: generates N bytes of input, hashes M times
// Measures and prints MB/second hash performance each time
function testSpeed (hashFn, N, M) {
  let startMs = new Date().getTime()

  const input = new Uint8Array(N)
  for (let i = 0; i < N; i++) {
    input[i] = i % 256
  }
  const genMs = new Date().getTime()
  console.log('Generated random input in ' + (genMs - startMs) + 'ms')
  startMs = genMs

  for (let i = 0; i < M; i++) {
    const hashHex = hashFn(input)
    const hashMs = new Date().getTime()
    const ms = hashMs - startMs
    startMs = hashMs
    console.log('Hashed in ' + ms + 'ms: ' + hashHex.substring(0, 20) + '...')
    console.log(
      Math.round((N / (1 << 20) / (ms / 1000)) * 100) / 100 + ' MB PER SECOND'
    )
  }
}

module.exports = {
  normalizeInput: normalizeInput,
  toHex: toHex,
  debugPrint: debugPrint,
  testSpeed: testSpeed
}

},{}]},{},[3])
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy5ucG0tcGFja2FnZXMvbGliL25vZGVfbW9kdWxlcy9icm93c2VyaWZ5L25vZGVfbW9kdWxlcy9icm93c2VyLXBhY2svX3ByZWx1ZGUuanMiLCJzcmMvYmxha2UyYi5qcyIsInNyYy9ibGFrZTJzLmpzIiwic3JjL2luZGV4LmpzIiwic3JjL3V0aWwuanMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7QUNBQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDN1dBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTs7QUNqV0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDZkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0EiLCJmaWxlIjoiZ2VuZXJhdGVkLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXNDb250ZW50IjpbIihmdW5jdGlvbigpe2Z1bmN0aW9uIHIoZSxuLHQpe2Z1bmN0aW9uIG8oaSxmKXtpZighbltpXSl7aWYoIWVbaV0pe3ZhciBjPVwiZnVuY3Rpb25cIj09dHlwZW9mIHJlcXVpcmUmJnJlcXVpcmU7aWYoIWYmJmMpcmV0dXJuIGMoaSwhMCk7aWYodSlyZXR1cm4gdShpLCEwKTt2YXIgYT1uZXcgRXJyb3IoXCJDYW5ub3QgZmluZCBtb2R1bGUgJ1wiK2krXCInXCIpO3Rocm93IGEuY29kZT1cIk1PRFVMRV9OT1RfRk9VTkRcIixhfXZhciBwPW5baV09e2V4cG9ydHM6e319O2VbaV1bMF0uY2FsbChwLmV4cG9ydHMsZnVuY3Rpb24ocil7dmFyIG49ZVtpXVsxXVtyXTtyZXR1cm4gbyhufHxyKX0scCxwLmV4cG9ydHMscixlLG4sdCl9cmV0dXJuIG5baV0uZXhwb3J0c31mb3IodmFyIHU9XCJmdW5jdGlvblwiPT10eXBlb2YgcmVxdWlyZSYmcmVxdWlyZSxpPTA7aTx0Lmxlbmd0aDtpKyspbyh0W2ldKTtyZXR1cm4gb31yZXR1cm4gcn0pKCkiLCIvLyBCbGFrZTJCIGluIHB1cmUgSmF2YXNjcmlwdFxuLy8gQWRhcHRlZCBmcm9tIHRoZSByZWZlcmVuY2UgaW1wbGVtZW50YXRpb24gaW4gUkZDNzY5M1xuLy8gUG9ydGVkIHRvIEphdmFzY3JpcHQgYnkgREMgLSBodHRwczovL2dpdGh1Yi5jb20vZGNwb3NjaFxuXG5jb25zdCB1dGlsID0gcmVxdWlyZSgnLi91dGlsJylcblxuLy8gNjQtYml0IHVuc2lnbmVkIGFkZGl0aW9uXG4vLyBTZXRzIHZbYSxhKzFdICs9IHZbYixiKzFdXG4vLyB2IHNob3VsZCBiZSBhIFVpbnQzMkFycmF5XG5mdW5jdGlvbiBBREQ2NEFBICh2LCBhLCBiKSB7XG4gIGNvbnN0IG8wID0gdlthXSArIHZbYl1cbiAgbGV0IG8xID0gdlthICsgMV0gKyB2W2IgKyAxXVxuICBpZiAobzAgPj0gMHgxMDAwMDAwMDApIHtcbiAgICBvMSsrXG4gIH1cbiAgdlthXSA9IG8wXG4gIHZbYSArIDFdID0gbzFcbn1cblxuLy8gNjQtYml0IHVuc2lnbmVkIGFkZGl0aW9uXG4vLyBTZXRzIHZbYSxhKzFdICs9IGJcbi8vIGIwIGlzIHRoZSBsb3cgMzIgYml0cyBvZiBiLCBiMSByZXByZXNlbnRzIHRoZSBoaWdoIDMyIGJpdHNcbmZ1bmN0aW9uIEFERDY0QUMgKHYsIGEsIGIwLCBiMSkge1xuICBsZXQgbzAgPSB2W2FdICsgYjBcbiAgaWYgKGIwIDwgMCkge1xuICAgIG8wICs9IDB4MTAwMDAwMDAwXG4gIH1cbiAgbGV0IG8xID0gdlthICsgMV0gKyBiMVxuICBpZiAobzAgPj0gMHgxMDAwMDAwMDApIHtcbiAgICBvMSsrXG4gIH1cbiAgdlthXSA9IG8wXG4gIHZbYSArIDFdID0gbzFcbn1cblxuLy8gTGl0dGxlLWVuZGlhbiBieXRlIGFjY2Vzc1xuZnVuY3Rpb24gQjJCX0dFVDMyIChhcnIsIGkpIHtcbiAgcmV0dXJuIGFycltpXSBeIChhcnJbaSArIDFdIDw8IDgpIF4gKGFycltpICsgMl0gPDwgMTYpIF4gKGFycltpICsgM10gPDwgMjQpXG59XG5cbi8vIEcgTWl4aW5nIGZ1bmN0aW9uXG4vLyBUaGUgUk9UUnMgYXJlIGlubGluZWQgZm9yIHNwZWVkXG5mdW5jdGlvbiBCMkJfRyAoYSwgYiwgYywgZCwgaXgsIGl5KSB7XG4gIGNvbnN0IHgwID0gbVtpeF1cbiAgY29uc3QgeDEgPSBtW2l4ICsgMV1cbiAgY29uc3QgeTAgPSBtW2l5XVxuICBjb25zdCB5MSA9IG1baXkgKyAxXVxuXG4gIEFERDY0QUEodiwgYSwgYikgLy8gdlthLGErMV0gKz0gdltiLGIrMV0gLi4uIGluIEpTIHdlIG11c3Qgc3RvcmUgYSB1aW50NjQgYXMgdHdvIHVpbnQzMnNcbiAgQURENjRBQyh2LCBhLCB4MCwgeDEpIC8vIHZbYSwgYSsxXSArPSB4IC4uLiB4MCBpcyB0aGUgbG93IDMyIGJpdHMgb2YgeCwgeDEgaXMgdGhlIGhpZ2ggMzIgYml0c1xuXG4gIC8vIHZbZCxkKzFdID0gKHZbZCxkKzFdIHhvciB2W2EsYSsxXSkgcm90YXRlZCB0byB0aGUgcmlnaHQgYnkgMzIgYml0c1xuICBsZXQgeG9yMCA9IHZbZF0gXiB2W2FdXG4gIGxldCB4b3IxID0gdltkICsgMV0gXiB2W2EgKyAxXVxuICB2W2RdID0geG9yMVxuICB2W2QgKyAxXSA9IHhvcjBcblxuICBBREQ2NEFBKHYsIGMsIGQpXG5cbiAgLy8gdltiLGIrMV0gPSAodltiLGIrMV0geG9yIHZbYyxjKzFdKSByb3RhdGVkIHJpZ2h0IGJ5IDI0IGJpdHNcbiAgeG9yMCA9IHZbYl0gXiB2W2NdXG4gIHhvcjEgPSB2W2IgKyAxXSBeIHZbYyArIDFdXG4gIHZbYl0gPSAoeG9yMCA+Pj4gMjQpIF4gKHhvcjEgPDwgOClcbiAgdltiICsgMV0gPSAoeG9yMSA+Pj4gMjQpIF4gKHhvcjAgPDwgOClcblxuICBBREQ2NEFBKHYsIGEsIGIpXG4gIEFERDY0QUModiwgYSwgeTAsIHkxKVxuXG4gIC8vIHZbZCxkKzFdID0gKHZbZCxkKzFdIHhvciB2W2EsYSsxXSkgcm90YXRlZCByaWdodCBieSAxNiBiaXRzXG4gIHhvcjAgPSB2W2RdIF4gdlthXVxuICB4b3IxID0gdltkICsgMV0gXiB2W2EgKyAxXVxuICB2W2RdID0gKHhvcjAgPj4+IDE2KSBeICh4b3IxIDw8IDE2KVxuICB2W2QgKyAxXSA9ICh4b3IxID4+PiAxNikgXiAoeG9yMCA8PCAxNilcblxuICBBREQ2NEFBKHYsIGMsIGQpXG5cbiAgLy8gdltiLGIrMV0gPSAodltiLGIrMV0geG9yIHZbYyxjKzFdKSByb3RhdGVkIHJpZ2h0IGJ5IDYzIGJpdHNcbiAgeG9yMCA9IHZbYl0gXiB2W2NdXG4gIHhvcjEgPSB2W2IgKyAxXSBeIHZbYyArIDFdXG4gIHZbYl0gPSAoeG9yMSA+Pj4gMzEpIF4gKHhvcjAgPDwgMSlcbiAgdltiICsgMV0gPSAoeG9yMCA+Pj4gMzEpIF4gKHhvcjEgPDwgMSlcbn1cblxuLy8gSW5pdGlhbGl6YXRpb24gVmVjdG9yXG5jb25zdCBCTEFLRTJCX0lWMzIgPSBuZXcgVWludDMyQXJyYXkoW1xuICAweGYzYmNjOTA4LCAweDZhMDllNjY3LCAweDg0Y2FhNzNiLCAweGJiNjdhZTg1LCAweGZlOTRmODJiLCAweDNjNmVmMzcyLFxuICAweDVmMWQzNmYxLCAweGE1NGZmNTNhLCAweGFkZTY4MmQxLCAweDUxMGU1MjdmLCAweDJiM2U2YzFmLCAweDliMDU2ODhjLFxuICAweGZiNDFiZDZiLCAweDFmODNkOWFiLCAweDEzN2UyMTc5LCAweDViZTBjZDE5XG5dKVxuXG5jb25zdCBTSUdNQTggPSBbXG4gIDAsIDEsIDIsIDMsIDQsIDUsIDYsIDcsIDgsIDksIDEwLCAxMSwgMTIsIDEzLCAxNCwgMTUsIDE0LCAxMCwgNCwgOCwgOSwgMTUsIDEzLFxuICA2LCAxLCAxMiwgMCwgMiwgMTEsIDcsIDUsIDMsIDExLCA4LCAxMiwgMCwgNSwgMiwgMTUsIDEzLCAxMCwgMTQsIDMsIDYsIDcsIDEsXG4gIDksIDQsIDcsIDksIDMsIDEsIDEzLCAxMiwgMTEsIDE0LCAyLCA2LCA1LCAxMCwgNCwgMCwgMTUsIDgsIDksIDAsIDUsIDcsIDIsIDQsXG4gIDEwLCAxNSwgMTQsIDEsIDExLCAxMiwgNiwgOCwgMywgMTMsIDIsIDEyLCA2LCAxMCwgMCwgMTEsIDgsIDMsIDQsIDEzLCA3LCA1LFxuICAxNSwgMTQsIDEsIDksIDEyLCA1LCAxLCAxNSwgMTQsIDEzLCA0LCAxMCwgMCwgNywgNiwgMywgOSwgMiwgOCwgMTEsIDEzLCAxMSwgNyxcbiAgMTQsIDEyLCAxLCAzLCA5LCA1LCAwLCAxNSwgNCwgOCwgNiwgMiwgMTAsIDYsIDE1LCAxNCwgOSwgMTEsIDMsIDAsIDgsIDEyLCAyLFxuICAxMywgNywgMSwgNCwgMTAsIDUsIDEwLCAyLCA4LCA0LCA3LCA2LCAxLCA1LCAxNSwgMTEsIDksIDE0LCAzLCAxMiwgMTMsIDAsIDAsXG4gIDEsIDIsIDMsIDQsIDUsIDYsIDcsIDgsIDksIDEwLCAxMSwgMTIsIDEzLCAxNCwgMTUsIDE0LCAxMCwgNCwgOCwgOSwgMTUsIDEzLCA2LFxuICAxLCAxMiwgMCwgMiwgMTEsIDcsIDUsIDNcbl1cblxuLy8gVGhlc2UgYXJlIG9mZnNldHMgaW50byBhIHVpbnQ2NCBidWZmZXIuXG4vLyBNdWx0aXBseSB0aGVtIGFsbCBieSAyIHRvIG1ha2UgdGhlbSBvZmZzZXRzIGludG8gYSB1aW50MzIgYnVmZmVyLFxuLy8gYmVjYXVzZSB0aGlzIGlzIEphdmFzY3JpcHQgYW5kIHdlIGRvbid0IGhhdmUgdWludDY0c1xuY29uc3QgU0lHTUE4MiA9IG5ldyBVaW50OEFycmF5KFxuICBTSUdNQTgubWFwKGZ1bmN0aW9uICh4KSB7XG4gICAgcmV0dXJuIHggKiAyXG4gIH0pXG4pXG5cbi8vIENvbXByZXNzaW9uIGZ1bmN0aW9uLiAnbGFzdCcgZmxhZyBpbmRpY2F0ZXMgbGFzdCBibG9jay5cbi8vIE5vdGUgd2UncmUgcmVwcmVzZW50aW5nIDE2IHVpbnQ2NHMgYXMgMzIgdWludDMyc1xuY29uc3QgdiA9IG5ldyBVaW50MzJBcnJheSgzMilcbmNvbnN0IG0gPSBuZXcgVWludDMyQXJyYXkoMzIpXG5mdW5jdGlvbiBibGFrZTJiQ29tcHJlc3MgKGN0eCwgbGFzdCkge1xuICBsZXQgaSA9IDBcblxuICAvLyBpbml0IHdvcmsgdmFyaWFibGVzXG4gIGZvciAoaSA9IDA7IGkgPCAxNjsgaSsrKSB7XG4gICAgdltpXSA9IGN0eC5oW2ldXG4gICAgdltpICsgMTZdID0gQkxBS0UyQl9JVjMyW2ldXG4gIH1cblxuICAvLyBsb3cgNjQgYml0cyBvZiBvZmZzZXRcbiAgdlsyNF0gPSB2WzI0XSBeIGN0eC50XG4gIHZbMjVdID0gdlsyNV0gXiAoY3R4LnQgLyAweDEwMDAwMDAwMClcbiAgLy8gaGlnaCA2NCBiaXRzIG5vdCBzdXBwb3J0ZWQsIG9mZnNldCBtYXkgbm90IGJlIGhpZ2hlciB0aGFuIDIqKjUzLTFcblxuICAvLyBsYXN0IGJsb2NrIGZsYWcgc2V0ID9cbiAgaWYgKGxhc3QpIHtcbiAgICB2WzI4XSA9IH52WzI4XVxuICAgIHZbMjldID0gfnZbMjldXG4gIH1cblxuICAvLyBnZXQgbGl0dGxlLWVuZGlhbiB3b3Jkc1xuICBmb3IgKGkgPSAwOyBpIDwgMzI7IGkrKykge1xuICAgIG1baV0gPSBCMkJfR0VUMzIoY3R4LmIsIDQgKiBpKVxuICB9XG5cbiAgLy8gdHdlbHZlIHJvdW5kcyBvZiBtaXhpbmdcbiAgLy8gdW5jb21tZW50IHRoZSBEZWJ1Z1ByaW50IGNhbGxzIHRvIGxvZyB0aGUgY29tcHV0YXRpb25cbiAgLy8gYW5kIG1hdGNoIHRoZSBSRkMgc2FtcGxlIGRvY3VtZW50YXRpb25cbiAgLy8gdXRpbC5kZWJ1Z1ByaW50KCcgICAgICAgICAgbVsxNl0nLCBtLCA2NClcbiAgZm9yIChpID0gMDsgaSA8IDEyOyBpKyspIHtcbiAgICAvLyB1dGlsLmRlYnVnUHJpbnQoJyAgIChpPScgKyAoaSA8IDEwID8gJyAnIDogJycpICsgaSArICcpIHZbMTZdJywgdiwgNjQpXG4gICAgQjJCX0coMCwgOCwgMTYsIDI0LCBTSUdNQTgyW2kgKiAxNiArIDBdLCBTSUdNQTgyW2kgKiAxNiArIDFdKVxuICAgIEIyQl9HKDIsIDEwLCAxOCwgMjYsIFNJR01BODJbaSAqIDE2ICsgMl0sIFNJR01BODJbaSAqIDE2ICsgM10pXG4gICAgQjJCX0coNCwgMTIsIDIwLCAyOCwgU0lHTUE4MltpICogMTYgKyA0XSwgU0lHTUE4MltpICogMTYgKyA1XSlcbiAgICBCMkJfRyg2LCAxNCwgMjIsIDMwLCBTSUdNQTgyW2kgKiAxNiArIDZdLCBTSUdNQTgyW2kgKiAxNiArIDddKVxuICAgIEIyQl9HKDAsIDEwLCAyMCwgMzAsIFNJR01BODJbaSAqIDE2ICsgOF0sIFNJR01BODJbaSAqIDE2ICsgOV0pXG4gICAgQjJCX0coMiwgMTIsIDIyLCAyNCwgU0lHTUE4MltpICogMTYgKyAxMF0sIFNJR01BODJbaSAqIDE2ICsgMTFdKVxuICAgIEIyQl9HKDQsIDE0LCAxNiwgMjYsIFNJR01BODJbaSAqIDE2ICsgMTJdLCBTSUdNQTgyW2kgKiAxNiArIDEzXSlcbiAgICBCMkJfRyg2LCA4LCAxOCwgMjgsIFNJR01BODJbaSAqIDE2ICsgMTRdLCBTSUdNQTgyW2kgKiAxNiArIDE1XSlcbiAgfVxuICAvLyB1dGlsLmRlYnVnUHJpbnQoJyAgIChpPTEyKSB2WzE2XScsIHYsIDY0KVxuXG4gIGZvciAoaSA9IDA7IGkgPCAxNjsgaSsrKSB7XG4gICAgY3R4LmhbaV0gPSBjdHguaFtpXSBeIHZbaV0gXiB2W2kgKyAxNl1cbiAgfVxuICAvLyB1dGlsLmRlYnVnUHJpbnQoJ2hbOF0nLCBjdHguaCwgNjQpXG59XG5cbi8vIHJldXNhYmxlIHBhcmFtZXRlckJsb2NrXG5jb25zdCBwYXJhbWV0ZXJCbG9jayA9IG5ldyBVaW50OEFycmF5KFtcbiAgMCxcbiAgMCxcbiAgMCxcbiAgMCwgLy8gIDA6IG91dGxlbiwga2V5bGVuLCBmYW5vdXQsIGRlcHRoXG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vICA0OiBsZWFmIGxlbmd0aCwgc2VxdWVudGlhbCBtb2RlXG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vICA4OiBub2RlIG9mZnNldFxuICAwLFxuICAwLFxuICAwLFxuICAwLCAvLyAxMjogbm9kZSBvZmZzZXRcbiAgMCxcbiAgMCxcbiAgMCxcbiAgMCwgLy8gMTY6IG5vZGUgZGVwdGgsIGlubmVyIGxlbmd0aCwgcmZ1XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDIwOiByZnVcbiAgMCxcbiAgMCxcbiAgMCxcbiAgMCwgLy8gMjQ6IHJmdVxuICAwLFxuICAwLFxuICAwLFxuICAwLCAvLyAyODogcmZ1XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDMyOiBzYWx0XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDM2OiBzYWx0XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDQwOiBzYWx0XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDQ0OiBzYWx0XG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAsIC8vIDQ4OiBwZXJzb25hbFxuICAwLFxuICAwLFxuICAwLFxuICAwLCAvLyA1MjogcGVyc29uYWxcbiAgMCxcbiAgMCxcbiAgMCxcbiAgMCwgLy8gNTY6IHBlcnNvbmFsXG4gIDAsXG4gIDAsXG4gIDAsXG4gIDAgLy8gNjA6IHBlcnNvbmFsXG5dKVxuXG4vLyBDcmVhdGVzIGEgQkxBS0UyYiBoYXNoaW5nIGNvbnRleHRcbi8vIFJlcXVpcmVzIGFuIG91dHB1dCBsZW5ndGggYmV0d2VlbiAxIGFuZCA2NCBieXRlc1xuLy8gVGFrZXMgYW4gb3B0aW9uYWwgVWludDhBcnJheSBrZXlcbi8vIFRha2VzIGFuIG9wdGluYWwgVWludDhBcnJheSBzYWx0XG4vLyBUYWtlcyBhbiBvcHRpbmFsIFVpbnQ4QXJyYXkgcGVyc29uYWxcbmZ1bmN0aW9uIGJsYWtlMmJJbml0IChvdXRsZW4sIGtleSwgc2FsdCwgcGVyc29uYWwpIHtcbiAgaWYgKG91dGxlbiA9PT0gMCB8fCBvdXRsZW4gPiA2NCkge1xuICAgIHRocm93IG5ldyBFcnJvcignSWxsZWdhbCBvdXRwdXQgbGVuZ3RoLCBleHBlY3RlZCAwIDwgbGVuZ3RoIDw9IDY0JylcbiAgfVxuICBpZiAoa2V5ICYmIGtleS5sZW5ndGggPiA2NCkge1xuICAgIHRocm93IG5ldyBFcnJvcignSWxsZWdhbCBrZXksIGV4cGVjdGVkIFVpbnQ4QXJyYXkgd2l0aCAwIDwgbGVuZ3RoIDw9IDY0JylcbiAgfVxuICBpZiAoc2FsdCAmJiBzYWx0Lmxlbmd0aCAhPT0gMTYpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ0lsbGVnYWwgc2FsdCwgZXhwZWN0ZWQgVWludDhBcnJheSB3aXRoIGxlbmd0aCBpcyAxNicpXG4gIH1cbiAgaWYgKHBlcnNvbmFsICYmIHBlcnNvbmFsLmxlbmd0aCAhPT0gMTYpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ0lsbGVnYWwgcGVyc29uYWwsIGV4cGVjdGVkIFVpbnQ4QXJyYXkgd2l0aCBsZW5ndGggaXMgMTYnKVxuICB9XG5cbiAgLy8gc3RhdGUsICdwYXJhbSBibG9jaydcbiAgY29uc3QgY3R4ID0ge1xuICAgIGI6IG5ldyBVaW50OEFycmF5KDEyOCksXG4gICAgaDogbmV3IFVpbnQzMkFycmF5KDE2KSxcbiAgICB0OiAwLCAvLyBpbnB1dCBjb3VudFxuICAgIGM6IDAsIC8vIHBvaW50ZXIgd2l0aGluIGJ1ZmZlclxuICAgIG91dGxlbjogb3V0bGVuIC8vIG91dHB1dCBsZW5ndGggaW4gYnl0ZXNcbiAgfVxuXG4gIC8vIGluaXRpYWxpemUgcGFyYW1ldGVyQmxvY2sgYmVmb3JlIHVzYWdlXG4gIHBhcmFtZXRlckJsb2NrLmZpbGwoMClcbiAgcGFyYW1ldGVyQmxvY2tbMF0gPSBvdXRsZW5cbiAgaWYgKGtleSkgcGFyYW1ldGVyQmxvY2tbMV0gPSBrZXkubGVuZ3RoXG4gIHBhcmFtZXRlckJsb2NrWzJdID0gMSAvLyBmYW5vdXRcbiAgcGFyYW1ldGVyQmxvY2tbM10gPSAxIC8vIGRlcHRoXG4gIGlmIChzYWx0KSBwYXJhbWV0ZXJCbG9jay5zZXQoc2FsdCwgMzIpXG4gIGlmIChwZXJzb25hbCkgcGFyYW1ldGVyQmxvY2suc2V0KHBlcnNvbmFsLCA0OClcblxuICAvLyBpbml0aWFsaXplIGhhc2ggc3RhdGVcbiAgZm9yIChsZXQgaSA9IDA7IGkgPCAxNjsgaSsrKSB7XG4gICAgY3R4LmhbaV0gPSBCTEFLRTJCX0lWMzJbaV0gXiBCMkJfR0VUMzIocGFyYW1ldGVyQmxvY2ssIGkgKiA0KVxuICB9XG5cbiAgLy8ga2V5IHRoZSBoYXNoLCBpZiBhcHBsaWNhYmxlXG4gIGlmIChrZXkpIHtcbiAgICBibGFrZTJiVXBkYXRlKGN0eCwga2V5KVxuICAgIC8vIGF0IHRoZSBlbmRcbiAgICBjdHguYyA9IDEyOFxuICB9XG5cbiAgcmV0dXJuIGN0eFxufVxuXG4vLyBVcGRhdGVzIGEgQkxBS0UyYiBzdHJlYW1pbmcgaGFzaFxuLy8gUmVxdWlyZXMgaGFzaCBjb250ZXh0IGFuZCBVaW50OEFycmF5IChieXRlIGFycmF5KVxuZnVuY3Rpb24gYmxha2UyYlVwZGF0ZSAoY3R4LCBpbnB1dCkge1xuICBmb3IgKGxldCBpID0gMDsgaSA8IGlucHV0Lmxlbmd0aDsgaSsrKSB7XG4gICAgaWYgKGN0eC5jID09PSAxMjgpIHtcbiAgICAgIC8vIGJ1ZmZlciBmdWxsID9cbiAgICAgIGN0eC50ICs9IGN0eC5jIC8vIGFkZCBjb3VudGVyc1xuICAgICAgYmxha2UyYkNvbXByZXNzKGN0eCwgZmFsc2UpIC8vIGNvbXByZXNzIChub3QgbGFzdClcbiAgICAgIGN0eC5jID0gMCAvLyBjb3VudGVyIHRvIHplcm9cbiAgICB9XG4gICAgY3R4LmJbY3R4LmMrK10gPSBpbnB1dFtpXVxuICB9XG59XG5cbi8vIENvbXBsZXRlcyBhIEJMQUtFMmIgc3RyZWFtaW5nIGhhc2hcbi8vIFJldHVybnMgYSBVaW50OEFycmF5IGNvbnRhaW5pbmcgdGhlIG1lc3NhZ2UgZGlnZXN0XG5mdW5jdGlvbiBibGFrZTJiRmluYWwgKGN0eCkge1xuICBjdHgudCArPSBjdHguYyAvLyBtYXJrIGxhc3QgYmxvY2sgb2Zmc2V0XG5cbiAgd2hpbGUgKGN0eC5jIDwgMTI4KSB7XG4gICAgLy8gZmlsbCB1cCB3aXRoIHplcm9zXG4gICAgY3R4LmJbY3R4LmMrK10gPSAwXG4gIH1cbiAgYmxha2UyYkNvbXByZXNzKGN0eCwgdHJ1ZSkgLy8gZmluYWwgYmxvY2sgZmxhZyA9IDFcblxuICAvLyBsaXR0bGUgZW5kaWFuIGNvbnZlcnQgYW5kIHN0b3JlXG4gIGNvbnN0IG91dCA9IG5ldyBVaW50OEFycmF5KGN0eC5vdXRsZW4pXG4gIGZvciAobGV0IGkgPSAwOyBpIDwgY3R4Lm91dGxlbjsgaSsrKSB7XG4gICAgb3V0W2ldID0gY3R4LmhbaSA+PiAyXSA+PiAoOCAqIChpICYgMykpXG4gIH1cbiAgcmV0dXJuIG91dFxufVxuXG4vLyBDb21wdXRlcyB0aGUgQkxBS0UyQiBoYXNoIG9mIGEgc3RyaW5nIG9yIGJ5dGUgYXJyYXksIGFuZCByZXR1cm5zIGEgVWludDhBcnJheVxuLy9cbi8vIFJldHVybnMgYSBuLWJ5dGUgVWludDhBcnJheVxuLy9cbi8vIFBhcmFtZXRlcnM6XG4vLyAtIGlucHV0IC0gdGhlIGlucHV0IGJ5dGVzLCBhcyBhIHN0cmluZywgQnVmZmVyIG9yIFVpbnQ4QXJyYXlcbi8vIC0ga2V5IC0gb3B0aW9uYWwga2V5IFVpbnQ4QXJyYXksIHVwIHRvIDY0IGJ5dGVzXG4vLyAtIG91dGxlbiAtIG9wdGlvbmFsIG91dHB1dCBsZW5ndGggaW4gYnl0ZXMsIGRlZmF1bHQgNjRcbi8vIC0gc2FsdCAtIG9wdGlvbmFsIHNhbHQgYnl0ZXMsIHN0cmluZywgQnVmZmVyIG9yIFVpbnQ4QXJyYXlcbi8vIC0gcGVyc29uYWwgLSBvcHRpb25hbCBwZXJzb25hbCBieXRlcywgc3RyaW5nLCBCdWZmZXIgb3IgVWludDhBcnJheVxuZnVuY3Rpb24gYmxha2UyYiAoaW5wdXQsIGtleSwgb3V0bGVuLCBzYWx0LCBwZXJzb25hbCkge1xuICAvLyBwcmVwcm9jZXNzIGlucHV0c1xuICBvdXRsZW4gPSBvdXRsZW4gfHwgNjRcbiAgaW5wdXQgPSB1dGlsLm5vcm1hbGl6ZUlucHV0KGlucHV0KVxuICBpZiAoc2FsdCkge1xuICAgIHNhbHQgPSB1dGlsLm5vcm1hbGl6ZUlucHV0KHNhbHQpXG4gIH1cbiAgaWYgKHBlcnNvbmFsKSB7XG4gICAgcGVyc29uYWwgPSB1dGlsLm5vcm1hbGl6ZUlucHV0KHBlcnNvbmFsKVxuICB9XG5cbiAgLy8gZG8gdGhlIG1hdGhcbiAgY29uc3QgY3R4ID0gYmxha2UyYkluaXQob3V0bGVuLCBrZXksIHNhbHQsIHBlcnNvbmFsKVxuICBibGFrZTJiVXBkYXRlKGN0eCwgaW5wdXQpXG4gIHJldHVybiBibGFrZTJiRmluYWwoY3R4KVxufVxuXG4vLyBDb21wdXRlcyB0aGUgQkxBS0UyQiBoYXNoIG9mIGEgc3RyaW5nIG9yIGJ5dGUgYXJyYXlcbi8vXG4vLyBSZXR1cm5zIGFuIG4tYnl0ZSBoYXNoIGluIGhleCwgYWxsIGxvd2VyY2FzZVxuLy9cbi8vIFBhcmFtZXRlcnM6XG4vLyAtIGlucHV0IC0gdGhlIGlucHV0IGJ5dGVzLCBhcyBhIHN0cmluZywgQnVmZmVyLCBvciBVaW50OEFycmF5XG4vLyAtIGtleSAtIG9wdGlvbmFsIGtleSBVaW50OEFycmF5LCB1cCB0byA2NCBieXRlc1xuLy8gLSBvdXRsZW4gLSBvcHRpb25hbCBvdXRwdXQgbGVuZ3RoIGluIGJ5dGVzLCBkZWZhdWx0IDY0XG4vLyAtIHNhbHQgLSBvcHRpb25hbCBzYWx0IGJ5dGVzLCBzdHJpbmcsIEJ1ZmZlciBvciBVaW50OEFycmF5XG4vLyAtIHBlcnNvbmFsIC0gb3B0aW9uYWwgcGVyc29uYWwgYnl0ZXMsIHN0cmluZywgQnVmZmVyIG9yIFVpbnQ4QXJyYXlcbmZ1bmN0aW9uIGJsYWtlMmJIZXggKGlucHV0LCBrZXksIG91dGxlbiwgc2FsdCwgcGVyc29uYWwpIHtcbiAgY29uc3Qgb3V0cHV0ID0gYmxha2UyYihpbnB1dCwga2V5LCBvdXRsZW4sIHNhbHQsIHBlcnNvbmFsKVxuICByZXR1cm4gdXRpbC50b0hleChvdXRwdXQpXG59XG5cbm1vZHVsZS5leHBvcnRzID0ge1xuICBibGFrZTJiOiBibGFrZTJiLFxuICBibGFrZTJiSGV4OiBibGFrZTJiSGV4LFxuICBibGFrZTJiSW5pdDogYmxha2UyYkluaXQsXG4gIGJsYWtlMmJVcGRhdGU6IGJsYWtlMmJVcGRhdGUsXG4gIGJsYWtlMmJGaW5hbDogYmxha2UyYkZpbmFsXG59XG4iLCIvLyBCTEFLRTJzIGhhc2ggZnVuY3Rpb24gaW4gcHVyZSBKYXZhc2NyaXB0XG4vLyBBZGFwdGVkIGZyb20gdGhlIHJlZmVyZW5jZSBpbXBsZW1lbnRhdGlvbiBpbiBSRkM3NjkzXG4vLyBQb3J0ZWQgdG8gSmF2YXNjcmlwdCBieSBEQyAtIGh0dHBzOi8vZ2l0aHViLmNvbS9kY3Bvc2NoXG5cbmNvbnN0IHV0aWwgPSByZXF1aXJlKCcuL3V0aWwnKVxuXG4vLyBMaXR0bGUtZW5kaWFuIGJ5dGUgYWNjZXNzLlxuLy8gRXhwZWN0cyBhIFVpbnQ4QXJyYXkgYW5kIGFuIGluZGV4XG4vLyBSZXR1cm5zIHRoZSBsaXR0bGUtZW5kaWFuIHVpbnQzMiBhdCB2W2kuLmkrM11cbmZ1bmN0aW9uIEIyU19HRVQzMiAodiwgaSkge1xuICByZXR1cm4gdltpXSBeICh2W2kgKyAxXSA8PCA4KSBeICh2W2kgKyAyXSA8PCAxNikgXiAodltpICsgM10gPDwgMjQpXG59XG5cbi8vIE1peGluZyBmdW5jdGlvbiBHLlxuZnVuY3Rpb24gQjJTX0cgKGEsIGIsIGMsIGQsIHgsIHkpIHtcbiAgdlthXSA9IHZbYV0gKyB2W2JdICsgeFxuICB2W2RdID0gUk9UUjMyKHZbZF0gXiB2W2FdLCAxNilcbiAgdltjXSA9IHZbY10gKyB2W2RdXG4gIHZbYl0gPSBST1RSMzIodltiXSBeIHZbY10sIDEyKVxuICB2W2FdID0gdlthXSArIHZbYl0gKyB5XG4gIHZbZF0gPSBST1RSMzIodltkXSBeIHZbYV0sIDgpXG4gIHZbY10gPSB2W2NdICsgdltkXVxuICB2W2JdID0gUk9UUjMyKHZbYl0gXiB2W2NdLCA3KVxufVxuXG4vLyAzMi1iaXQgcmlnaHQgcm90YXRpb25cbi8vIHggc2hvdWxkIGJlIGEgdWludDMyXG4vLyB5IG11c3QgYmUgYmV0d2VlbiAxIGFuZCAzMSwgaW5jbHVzaXZlXG5mdW5jdGlvbiBST1RSMzIgKHgsIHkpIHtcbiAgcmV0dXJuICh4ID4+PiB5KSBeICh4IDw8ICgzMiAtIHkpKVxufVxuXG4vLyBJbml0aWFsaXphdGlvbiBWZWN0b3IuXG5jb25zdCBCTEFLRTJTX0lWID0gbmV3IFVpbnQzMkFycmF5KFtcbiAgMHg2YTA5ZTY2NyxcbiAgMHhiYjY3YWU4NSxcbiAgMHgzYzZlZjM3MixcbiAgMHhhNTRmZjUzYSxcbiAgMHg1MTBlNTI3ZixcbiAgMHg5YjA1Njg4YyxcbiAgMHgxZjgzZDlhYixcbiAgMHg1YmUwY2QxOVxuXSlcblxuY29uc3QgU0lHTUEgPSBuZXcgVWludDhBcnJheShbXG4gIDAsXG4gIDEsXG4gIDIsXG4gIDMsXG4gIDQsXG4gIDUsXG4gIDYsXG4gIDcsXG4gIDgsXG4gIDksXG4gIDEwLFxuICAxMSxcbiAgMTIsXG4gIDEzLFxuICAxNCxcbiAgMTUsXG4gIDE0LFxuICAxMCxcbiAgNCxcbiAgOCxcbiAgOSxcbiAgMTUsXG4gIDEzLFxuICA2LFxuICAxLFxuICAxMixcbiAgMCxcbiAgMixcbiAgMTEsXG4gIDcsXG4gIDUsXG4gIDMsXG4gIDExLFxuICA4LFxuICAxMixcbiAgMCxcbiAgNSxcbiAgMixcbiAgMTUsXG4gIDEzLFxuICAxMCxcbiAgMTQsXG4gIDMsXG4gIDYsXG4gIDcsXG4gIDEsXG4gIDksXG4gIDQsXG4gIDcsXG4gIDksXG4gIDMsXG4gIDEsXG4gIDEzLFxuICAxMixcbiAgMTEsXG4gIDE0LFxuICAyLFxuICA2LFxuICA1LFxuICAxMCxcbiAgNCxcbiAgMCxcbiAgMTUsXG4gIDgsXG4gIDksXG4gIDAsXG4gIDUsXG4gIDcsXG4gIDIsXG4gIDQsXG4gIDEwLFxuICAxNSxcbiAgMTQsXG4gIDEsXG4gIDExLFxuICAxMixcbiAgNixcbiAgOCxcbiAgMyxcbiAgMTMsXG4gIDIsXG4gIDEyLFxuICA2LFxuICAxMCxcbiAgMCxcbiAgMTEsXG4gIDgsXG4gIDMsXG4gIDQsXG4gIDEzLFxuICA3LFxuICA1LFxuICAxNSxcbiAgMTQsXG4gIDEsXG4gIDksXG4gIDEyLFxuICA1LFxuICAxLFxuICAxNSxcbiAgMTQsXG4gIDEzLFxuICA0LFxuICAxMCxcbiAgMCxcbiAgNyxcbiAgNixcbiAgMyxcbiAgOSxcbiAgMixcbiAgOCxcbiAgMTEsXG4gIDEzLFxuICAxMSxcbiAgNyxcbiAgMTQsXG4gIDEyLFxuICAxLFxuICAzLFxuICA5LFxuICA1LFxuICAwLFxuICAxNSxcbiAgNCxcbiAgOCxcbiAgNixcbiAgMixcbiAgMTAsXG4gIDYsXG4gIDE1LFxuICAxNCxcbiAgOSxcbiAgMTEsXG4gIDMsXG4gIDAsXG4gIDgsXG4gIDEyLFxuICAyLFxuICAxMyxcbiAgNyxcbiAgMSxcbiAgNCxcbiAgMTAsXG4gIDUsXG4gIDEwLFxuICAyLFxuICA4LFxuICA0LFxuICA3LFxuICA2LFxuICAxLFxuICA1LFxuICAxNSxcbiAgMTEsXG4gIDksXG4gIDE0LFxuICAzLFxuICAxMixcbiAgMTMsXG4gIDBcbl0pXG5cbi8vIENvbXByZXNzaW9uIGZ1bmN0aW9uLiBcImxhc3RcIiBmbGFnIGluZGljYXRlcyBsYXN0IGJsb2NrXG5jb25zdCB2ID0gbmV3IFVpbnQzMkFycmF5KDE2KVxuY29uc3QgbSA9IG5ldyBVaW50MzJBcnJheSgxNilcbmZ1bmN0aW9uIGJsYWtlMnNDb21wcmVzcyAoY3R4LCBsYXN0KSB7XG4gIGxldCBpID0gMFxuICBmb3IgKGkgPSAwOyBpIDwgODsgaSsrKSB7XG4gICAgLy8gaW5pdCB3b3JrIHZhcmlhYmxlc1xuICAgIHZbaV0gPSBjdHguaFtpXVxuICAgIHZbaSArIDhdID0gQkxBS0UyU19JVltpXVxuICB9XG5cbiAgdlsxMl0gXj0gY3R4LnQgLy8gbG93IDMyIGJpdHMgb2Ygb2Zmc2V0XG4gIHZbMTNdIF49IGN0eC50IC8gMHgxMDAwMDAwMDAgLy8gaGlnaCAzMiBiaXRzXG4gIGlmIChsYXN0KSB7XG4gICAgLy8gbGFzdCBibG9jayBmbGFnIHNldCA/XG4gICAgdlsxNF0gPSB+dlsxNF1cbiAgfVxuXG4gIGZvciAoaSA9IDA7IGkgPCAxNjsgaSsrKSB7XG4gICAgLy8gZ2V0IGxpdHRsZS1lbmRpYW4gd29yZHNcbiAgICBtW2ldID0gQjJTX0dFVDMyKGN0eC5iLCA0ICogaSlcbiAgfVxuXG4gIC8vIHRlbiByb3VuZHMgb2YgbWl4aW5nXG4gIC8vIHVuY29tbWVudCB0aGUgRGVidWdQcmludCBjYWxscyB0byBsb2cgdGhlIGNvbXB1dGF0aW9uXG4gIC8vIGFuZCBtYXRjaCB0aGUgUkZDIHNhbXBsZSBkb2N1bWVudGF0aW9uXG4gIC8vIHV0aWwuZGVidWdQcmludCgnICAgICAgICAgIG1bMTZdJywgbSwgMzIpXG4gIGZvciAoaSA9IDA7IGkgPCAxMDsgaSsrKSB7XG4gICAgLy8gdXRpbC5kZWJ1Z1ByaW50KCcgICAoaT0nICsgaSArICcpICB2WzE2XScsIHYsIDMyKVxuICAgIEIyU19HKDAsIDQsIDgsIDEyLCBtW1NJR01BW2kgKiAxNiArIDBdXSwgbVtTSUdNQVtpICogMTYgKyAxXV0pXG4gICAgQjJTX0coMSwgNSwgOSwgMTMsIG1bU0lHTUFbaSAqIDE2ICsgMl1dLCBtW1NJR01BW2kgKiAxNiArIDNdXSlcbiAgICBCMlNfRygyLCA2LCAxMCwgMTQsIG1bU0lHTUFbaSAqIDE2ICsgNF1dLCBtW1NJR01BW2kgKiAxNiArIDVdXSlcbiAgICBCMlNfRygzLCA3LCAxMSwgMTUsIG1bU0lHTUFbaSAqIDE2ICsgNl1dLCBtW1NJR01BW2kgKiAxNiArIDddXSlcbiAgICBCMlNfRygwLCA1LCAxMCwgMTUsIG1bU0lHTUFbaSAqIDE2ICsgOF1dLCBtW1NJR01BW2kgKiAxNiArIDldXSlcbiAgICBCMlNfRygxLCA2LCAxMSwgMTIsIG1bU0lHTUFbaSAqIDE2ICsgMTBdXSwgbVtTSUdNQVtpICogMTYgKyAxMV1dKVxuICAgIEIyU19HKDIsIDcsIDgsIDEzLCBtW1NJR01BW2kgKiAxNiArIDEyXV0sIG1bU0lHTUFbaSAqIDE2ICsgMTNdXSlcbiAgICBCMlNfRygzLCA0LCA5LCAxNCwgbVtTSUdNQVtpICogMTYgKyAxNF1dLCBtW1NJR01BW2kgKiAxNiArIDE1XV0pXG4gIH1cbiAgLy8gdXRpbC5kZWJ1Z1ByaW50KCcgICAoaT0xMCkgdlsxNl0nLCB2LCAzMilcblxuICBmb3IgKGkgPSAwOyBpIDwgODsgaSsrKSB7XG4gICAgY3R4LmhbaV0gXj0gdltpXSBeIHZbaSArIDhdXG4gIH1cbiAgLy8gdXRpbC5kZWJ1Z1ByaW50KCdoWzhdJywgY3R4LmgsIDMyKVxufVxuXG4vLyBDcmVhdGVzIGEgQkxBS0UycyBoYXNoaW5nIGNvbnRleHRcbi8vIFJlcXVpcmVzIGFuIG91dHB1dCBsZW5ndGggYmV0d2VlbiAxIGFuZCAzMiBieXRlc1xuLy8gVGFrZXMgYW4gb3B0aW9uYWwgVWludDhBcnJheSBrZXlcbmZ1bmN0aW9uIGJsYWtlMnNJbml0IChvdXRsZW4sIGtleSkge1xuICBpZiAoIShvdXRsZW4gPiAwICYmIG91dGxlbiA8PSAzMikpIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ0luY29ycmVjdCBvdXRwdXQgbGVuZ3RoLCBzaG91bGQgYmUgaW4gWzEsIDMyXScpXG4gIH1cbiAgY29uc3Qga2V5bGVuID0ga2V5ID8ga2V5Lmxlbmd0aCA6IDBcbiAgaWYgKGtleSAmJiAhKGtleWxlbiA+IDAgJiYga2V5bGVuIDw9IDMyKSkge1xuICAgIHRocm93IG5ldyBFcnJvcignSW5jb3JyZWN0IGtleSBsZW5ndGgsIHNob3VsZCBiZSBpbiBbMSwgMzJdJylcbiAgfVxuXG4gIGNvbnN0IGN0eCA9IHtcbiAgICBoOiBuZXcgVWludDMyQXJyYXkoQkxBS0UyU19JViksIC8vIGhhc2ggc3RhdGVcbiAgICBiOiBuZXcgVWludDhBcnJheSg2NCksIC8vIGlucHV0IGJsb2NrXG4gICAgYzogMCwgLy8gcG9pbnRlciB3aXRoaW4gYmxvY2tcbiAgICB0OiAwLCAvLyBpbnB1dCBjb3VudFxuICAgIG91dGxlbjogb3V0bGVuIC8vIG91dHB1dCBsZW5ndGggaW4gYnl0ZXNcbiAgfVxuICBjdHguaFswXSBePSAweDAxMDEwMDAwIF4gKGtleWxlbiA8PCA4KSBeIG91dGxlblxuXG4gIGlmIChrZXlsZW4gPiAwKSB7XG4gICAgYmxha2Uyc1VwZGF0ZShjdHgsIGtleSlcbiAgICBjdHguYyA9IDY0IC8vIGF0IHRoZSBlbmRcbiAgfVxuXG4gIHJldHVybiBjdHhcbn1cblxuLy8gVXBkYXRlcyBhIEJMQUtFMnMgc3RyZWFtaW5nIGhhc2hcbi8vIFJlcXVpcmVzIGhhc2ggY29udGV4dCBhbmQgVWludDhBcnJheSAoYnl0ZSBhcnJheSlcbmZ1bmN0aW9uIGJsYWtlMnNVcGRhdGUgKGN0eCwgaW5wdXQpIHtcbiAgZm9yIChsZXQgaSA9IDA7IGkgPCBpbnB1dC5sZW5ndGg7IGkrKykge1xuICAgIGlmIChjdHguYyA9PT0gNjQpIHtcbiAgICAgIC8vIGJ1ZmZlciBmdWxsID9cbiAgICAgIGN0eC50ICs9IGN0eC5jIC8vIGFkZCBjb3VudGVyc1xuICAgICAgYmxha2Uyc0NvbXByZXNzKGN0eCwgZmFsc2UpIC8vIGNvbXByZXNzIChub3QgbGFzdClcbiAgICAgIGN0eC5jID0gMCAvLyBjb3VudGVyIHRvIHplcm9cbiAgICB9XG4gICAgY3R4LmJbY3R4LmMrK10gPSBpbnB1dFtpXVxuICB9XG59XG5cbi8vIENvbXBsZXRlcyBhIEJMQUtFMnMgc3RyZWFtaW5nIGhhc2hcbi8vIFJldHVybnMgYSBVaW50OEFycmF5IGNvbnRhaW5pbmcgdGhlIG1lc3NhZ2UgZGlnZXN0XG5mdW5jdGlvbiBibGFrZTJzRmluYWwgKGN0eCkge1xuICBjdHgudCArPSBjdHguYyAvLyBtYXJrIGxhc3QgYmxvY2sgb2Zmc2V0XG4gIHdoaWxlIChjdHguYyA8IDY0KSB7XG4gICAgLy8gZmlsbCB1cCB3aXRoIHplcm9zXG4gICAgY3R4LmJbY3R4LmMrK10gPSAwXG4gIH1cbiAgYmxha2Uyc0NvbXByZXNzKGN0eCwgdHJ1ZSkgLy8gZmluYWwgYmxvY2sgZmxhZyA9IDFcblxuICAvLyBsaXR0bGUgZW5kaWFuIGNvbnZlcnQgYW5kIHN0b3JlXG4gIGNvbnN0IG91dCA9IG5ldyBVaW50OEFycmF5KGN0eC5vdXRsZW4pXG4gIGZvciAobGV0IGkgPSAwOyBpIDwgY3R4Lm91dGxlbjsgaSsrKSB7XG4gICAgb3V0W2ldID0gKGN0eC5oW2kgPj4gMl0gPj4gKDggKiAoaSAmIDMpKSkgJiAweGZmXG4gIH1cbiAgcmV0dXJuIG91dFxufVxuXG4vLyBDb21wdXRlcyB0aGUgQkxBS0UyUyBoYXNoIG9mIGEgc3RyaW5nIG9yIGJ5dGUgYXJyYXksIGFuZCByZXR1cm5zIGEgVWludDhBcnJheVxuLy9cbi8vIFJldHVybnMgYSBuLWJ5dGUgVWludDhBcnJheVxuLy9cbi8vIFBhcmFtZXRlcnM6XG4vLyAtIGlucHV0IC0gdGhlIGlucHV0IGJ5dGVzLCBhcyBhIHN0cmluZywgQnVmZmVyLCBvciBVaW50OEFycmF5XG4vLyAtIGtleSAtIG9wdGlvbmFsIGtleSBVaW50OEFycmF5LCB1cCB0byAzMiBieXRlc1xuLy8gLSBvdXRsZW4gLSBvcHRpb25hbCBvdXRwdXQgbGVuZ3RoIGluIGJ5dGVzLCBkZWZhdWx0IDY0XG5mdW5jdGlvbiBibGFrZTJzIChpbnB1dCwga2V5LCBvdXRsZW4pIHtcbiAgLy8gcHJlcHJvY2VzcyBpbnB1dHNcbiAgb3V0bGVuID0gb3V0bGVuIHx8IDMyXG4gIGlucHV0ID0gdXRpbC5ub3JtYWxpemVJbnB1dChpbnB1dClcblxuICAvLyBkbyB0aGUgbWF0aFxuICBjb25zdCBjdHggPSBibGFrZTJzSW5pdChvdXRsZW4sIGtleSlcbiAgYmxha2Uyc1VwZGF0ZShjdHgsIGlucHV0KVxuICByZXR1cm4gYmxha2Uyc0ZpbmFsKGN0eClcbn1cblxuLy8gQ29tcHV0ZXMgdGhlIEJMQUtFMlMgaGFzaCBvZiBhIHN0cmluZyBvciBieXRlIGFycmF5XG4vL1xuLy8gUmV0dXJucyBhbiBuLWJ5dGUgaGFzaCBpbiBoZXgsIGFsbCBsb3dlcmNhc2Vcbi8vXG4vLyBQYXJhbWV0ZXJzOlxuLy8gLSBpbnB1dCAtIHRoZSBpbnB1dCBieXRlcywgYXMgYSBzdHJpbmcsIEJ1ZmZlciwgb3IgVWludDhBcnJheVxuLy8gLSBrZXkgLSBvcHRpb25hbCBrZXkgVWludDhBcnJheSwgdXAgdG8gMzIgYnl0ZXNcbi8vIC0gb3V0bGVuIC0gb3B0aW9uYWwgb3V0cHV0IGxlbmd0aCBpbiBieXRlcywgZGVmYXVsdCA2NFxuZnVuY3Rpb24gYmxha2Uyc0hleCAoaW5wdXQsIGtleSwgb3V0bGVuKSB7XG4gIGNvbnN0IG91dHB1dCA9IGJsYWtlMnMoaW5wdXQsIGtleSwgb3V0bGVuKVxuICByZXR1cm4gdXRpbC50b0hleChvdXRwdXQpXG59XG5cbm1vZHVsZS5leHBvcnRzID0ge1xuICBibGFrZTJzOiBibGFrZTJzLFxuICBibGFrZTJzSGV4OiBibGFrZTJzSGV4LFxuICBibGFrZTJzSW5pdDogYmxha2Uyc0luaXQsXG4gIGJsYWtlMnNVcGRhdGU6IGJsYWtlMnNVcGRhdGUsXG4gIGJsYWtlMnNGaW5hbDogYmxha2Uyc0ZpbmFsXG59XG4iLCJjb25zdCBiMmIgPSByZXF1aXJlKCcuL2JsYWtlMmInKVxuY29uc3QgYjJzID0gcmVxdWlyZSgnLi9ibGFrZTJzJylcblxubW9kdWxlLmV4cG9ydHMgPSB7XG4gIGJsYWtlMmI6IGIyYi5ibGFrZTJiLFxuICBibGFrZTJiSGV4OiBiMmIuYmxha2UyYkhleCxcbiAgYmxha2UyYkluaXQ6IGIyYi5ibGFrZTJiSW5pdCxcbiAgYmxha2UyYlVwZGF0ZTogYjJiLmJsYWtlMmJVcGRhdGUsXG4gIGJsYWtlMmJGaW5hbDogYjJiLmJsYWtlMmJGaW5hbCxcbiAgYmxha2UyczogYjJzLmJsYWtlMnMsXG4gIGJsYWtlMnNIZXg6IGIycy5ibGFrZTJzSGV4LFxuICBibGFrZTJzSW5pdDogYjJzLmJsYWtlMnNJbml0LFxuICBibGFrZTJzVXBkYXRlOiBiMnMuYmxha2Uyc1VwZGF0ZSxcbiAgYmxha2Uyc0ZpbmFsOiBiMnMuYmxha2Uyc0ZpbmFsXG59XG4iLCJjb25zdCBFUlJPUl9NU0dfSU5QVVQgPSAnSW5wdXQgbXVzdCBiZSBhbiBzdHJpbmcsIEJ1ZmZlciBvciBVaW50OEFycmF5J1xuXG4vLyBGb3IgY29udmVuaWVuY2UsIGxldCBwZW9wbGUgaGFzaCBhIHN0cmluZywgbm90IGp1c3QgYSBVaW50OEFycmF5XG5mdW5jdGlvbiBub3JtYWxpemVJbnB1dCAoaW5wdXQpIHtcbiAgbGV0IHJldFxuICBpZiAoaW5wdXQgaW5zdGFuY2VvZiBVaW50OEFycmF5KSB7XG4gICAgcmV0ID0gaW5wdXRcbiAgfSBlbHNlIGlmICh0eXBlb2YgaW5wdXQgPT09ICdzdHJpbmcnKSB7XG4gICAgY29uc3QgZW5jb2RlciA9IG5ldyBUZXh0RW5jb2RlcigpXG4gICAgcmV0ID0gZW5jb2Rlci5lbmNvZGUoaW5wdXQpXG4gIH0gZWxzZSB7XG4gICAgdGhyb3cgbmV3IEVycm9yKEVSUk9SX01TR19JTlBVVClcbiAgfVxuICByZXR1cm4gcmV0XG59XG5cbi8vIENvbnZlcnRzIGEgVWludDhBcnJheSB0byBhIGhleGFkZWNpbWFsIHN0cmluZ1xuLy8gRm9yIGV4YW1wbGUsIHRvSGV4KFsyNTUsIDAsIDI1NV0pIHJldHVybnMgXCJmZjAwZmZcIlxuZnVuY3Rpb24gdG9IZXggKGJ5dGVzKSB7XG4gIHJldHVybiBBcnJheS5wcm90b3R5cGUubWFwXG4gICAgLmNhbGwoYnl0ZXMsIGZ1bmN0aW9uIChuKSB7XG4gICAgICByZXR1cm4gKG4gPCAxNiA/ICcwJyA6ICcnKSArIG4udG9TdHJpbmcoMTYpXG4gICAgfSlcbiAgICAuam9pbignJylcbn1cblxuLy8gQ29udmVydHMgYW55IHZhbHVlIGluIFswLi4uMl4zMi0xXSB0byBhbiA4LWNoYXJhY3RlciBoZXggc3RyaW5nXG5mdW5jdGlvbiB1aW50MzJUb0hleCAodmFsKSB7XG4gIHJldHVybiAoMHgxMDAwMDAwMDAgKyB2YWwpLnRvU3RyaW5nKDE2KS5zdWJzdHJpbmcoMSlcbn1cblxuLy8gRm9yIGRlYnVnZ2luZzogcHJpbnRzIG91dCBoYXNoIHN0YXRlIGluIHRoZSBzYW1lIGZvcm1hdCBhcyB0aGUgUkZDXG4vLyBzYW1wbGUgY29tcHV0YXRpb24gZXhhY3RseSwgc28gdGhhdCB5b3UgY2FuIGRpZmZcbmZ1bmN0aW9uIGRlYnVnUHJpbnQgKGxhYmVsLCBhcnIsIHNpemUpIHtcbiAgbGV0IG1zZyA9ICdcXG4nICsgbGFiZWwgKyAnID0gJ1xuICBmb3IgKGxldCBpID0gMDsgaSA8IGFyci5sZW5ndGg7IGkgKz0gMikge1xuICAgIGlmIChzaXplID09PSAzMikge1xuICAgICAgbXNnICs9IHVpbnQzMlRvSGV4KGFycltpXSkudG9VcHBlckNhc2UoKVxuICAgICAgbXNnICs9ICcgJ1xuICAgICAgbXNnICs9IHVpbnQzMlRvSGV4KGFycltpICsgMV0pLnRvVXBwZXJDYXNlKClcbiAgICB9IGVsc2UgaWYgKHNpemUgPT09IDY0KSB7XG4gICAgICBtc2cgKz0gdWludDMyVG9IZXgoYXJyW2kgKyAxXSkudG9VcHBlckNhc2UoKVxuICAgICAgbXNnICs9IHVpbnQzMlRvSGV4KGFycltpXSkudG9VcHBlckNhc2UoKVxuICAgIH0gZWxzZSB0aHJvdyBuZXcgRXJyb3IoJ0ludmFsaWQgc2l6ZSAnICsgc2l6ZSlcbiAgICBpZiAoaSAlIDYgPT09IDQpIHtcbiAgICAgIG1zZyArPSAnXFxuJyArIG5ldyBBcnJheShsYWJlbC5sZW5ndGggKyA0KS5qb2luKCcgJylcbiAgICB9IGVsc2UgaWYgKGkgPCBhcnIubGVuZ3RoIC0gMikge1xuICAgICAgbXNnICs9ICcgJ1xuICAgIH1cbiAgfVxuICBjb25zb2xlLmxvZyhtc2cpXG59XG5cbi8vIEZvciBwZXJmb3JtYW5jZSB0ZXN0aW5nOiBnZW5lcmF0ZXMgTiBieXRlcyBvZiBpbnB1dCwgaGFzaGVzIE0gdGltZXNcbi8vIE1lYXN1cmVzIGFuZCBwcmludHMgTUIvc2Vjb25kIGhhc2ggcGVyZm9ybWFuY2UgZWFjaCB0aW1lXG5mdW5jdGlvbiB0ZXN0U3BlZWQgKGhhc2hGbiwgTiwgTSkge1xuICBsZXQgc3RhcnRNcyA9IG5ldyBEYXRlKCkuZ2V0VGltZSgpXG5cbiAgY29uc3QgaW5wdXQgPSBuZXcgVWludDhBcnJheShOKVxuICBmb3IgKGxldCBpID0gMDsgaSA8IE47IGkrKykge1xuICAgIGlucHV0W2ldID0gaSAlIDI1NlxuICB9XG4gIGNvbnN0IGdlbk1zID0gbmV3IERhdGUoKS5nZXRUaW1lKClcbiAgY29uc29sZS5sb2coJ0dlbmVyYXRlZCByYW5kb20gaW5wdXQgaW4gJyArIChnZW5NcyAtIHN0YXJ0TXMpICsgJ21zJylcbiAgc3RhcnRNcyA9IGdlbk1zXG5cbiAgZm9yIChsZXQgaSA9IDA7IGkgPCBNOyBpKyspIHtcbiAgICBjb25zdCBoYXNoSGV4ID0gaGFzaEZuKGlucHV0KVxuICAgIGNvbnN0IGhhc2hNcyA9IG5ldyBEYXRlKCkuZ2V0VGltZSgpXG4gICAgY29uc3QgbXMgPSBoYXNoTXMgLSBzdGFydE1zXG4gICAgc3RhcnRNcyA9IGhhc2hNc1xuICAgIGNvbnNvbGUubG9nKCdIYXNoZWQgaW4gJyArIG1zICsgJ21zOiAnICsgaGFzaEhleC5zdWJzdHJpbmcoMCwgMjApICsgJy4uLicpXG4gICAgY29uc29sZS5sb2coXG4gICAgICBNYXRoLnJvdW5kKChOIC8gKDEgPDwgMjApIC8gKG1zIC8gMTAwMCkpICogMTAwKSAvIDEwMCArICcgTUIgUEVSIFNFQ09ORCdcbiAgICApXG4gIH1cbn1cblxubW9kdWxlLmV4cG9ydHMgPSB7XG4gIG5vcm1hbGl6ZUlucHV0OiBub3JtYWxpemVJbnB1dCxcbiAgdG9IZXg6IHRvSGV4LFxuICBkZWJ1Z1ByaW50OiBkZWJ1Z1ByaW50LFxuICB0ZXN0U3BlZWQ6IHRlc3RTcGVlZFxufVxuIl19
