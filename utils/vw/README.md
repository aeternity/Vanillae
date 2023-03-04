# vw = vanillae wallet

This started as (/is currently) a scratchpad for TypeScript code. Any time I
have a thing I need to write in TypeScript, I would write it in Erlang first,
and then adapt my Erlang solution to TypeScript.

This will probably eventually be a reference Aeternity wallet implementation in
Erlang for people who want to make Aeternity wallets.

Thus, it contains many (intentional) inefficiencies.  For instance, the Base64
bytestring-to-ASCII encoding is implemented in the `base64` module of the
Erlang standard library.  vw uses an implementation written by hand in Erlang
(`src/vb64.erl`), again because I have to implement it by hand in TypeScript,
and want a reference in a more sane environment.

Generally: clarity is favored over performance.

ISC licenesed like everything else in Aeternity.
