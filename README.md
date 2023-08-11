# Vanillae

## How to use this project


## Downloads


## Quick Links

-   [Vanillae Grant Proposal][grant-proposal]
-   [Vanillae Files][vanillae-files], miscellaneous explainers mostly for
    things that should take 1 hour to learn but in practice take 11 hours
    because information has to be synthesized from multiple sources

-   Video content
    -   [Vanillae Rumble Channel](https://rumble.com/c/c-3509606)
    -   [Vanillae Odysee Channel](https://odysee.com/@VanillaeProject:0)
    -   [Vanillae YouTube Channel](https://www.youtube.com/@vanillaeproject)
-   Miscellany:
    -   [Aegora.jp][aegora], original impetus for product
    -   [Aeternity JS SDK][aesdk], which you want for more complicated use cases

## About this project

Aeternity is a high-performance open-source decentralized payment network.
Vanillae is a tool suite for using Aeternity.  There is both developer-facing
tooling and user-facing tooling. There is also miscellaneous documentation, and
explainers etc.

The two developers are Craig Everett (@zxq9) and Peter Harpending (@pharpend).
Vanillae grew out of [Aegora.jp][aegora].  We made a simple
e-commerce store using Aeternity as the payment system.  We encountered a lot
of rough edges in Aeternity's tooling, so developed our own, and
open-sourced it.  The Aeternity Foundation chose to pay us to grow out our
toolset, and that became this project.  You can read the [grant
proposal][grant-proposal] for more information.

Often during this process we ran into some thing that really should take 1 hour
to learn about, but in practice takes 11 hours because you have to synthesize
information from 100 different sources.  In many of those cases, we went to the
trouble to write it all out in one place (that's the [Vanillae
Files][vanillae-files]), and in some cases there are videos on our
YT/Rumble/Odysee channels.

The goal is to write an explainer doc and have one or more videos for each
instance of information synthesis paralysis.

Generally, we write tools that we want to use.  So

-   Functionality tends to be pretty minimal.
-   The only usage cases covered are those we have actually encountered.
-   When we encounter a tradeoff between simplicity and flexibility, we
    generally have chosen simplicity.
-   Our primary deployment target is the [zx/zomp ecosystem][zx-zomp], not the
    Node/NPM ecosystem

    For instance, this means that our JS/TS code is meant to run inside a
    browser, and we don't give any thought to running it as server-side JS,
    because that's not something we would ever do ourselves.

    We're happy to support the Node/NPM ecosystem on a case-by-case basis as
    long as it doesn't force us to reduce the quality of our product.

If you have a more complicated use case or live in the Node/NPM ecosystem, you
want the [Aeternity JavaScript SDK][aesdk] The SDK is a Swiss Army Knife.
Vanillae is just a set of knives, each of which was designed for a specific
cutting task.


## Directory Structure

```
art/                    Project icons, etc
bindings/               FLAGSHIP: APIs to talk to the AE blockchain from app backend
    erlang/                 [ERL] Erlang Vanillae Bindings
docs/                   Explainer docs
    baseN/                  Base64 versus Base58 (they are fundamentally different ideas!)
    ecc/                    Elliptic Curve Cryptography
    jex/                    Jex: our TS/JS packaging tool we use instead of NPM
    kek/                    The Keccak/SHA-3 Algorithm explained
    npm-misc/               How to install NPM on Linux such that you never have to use `sudo npm`
    rlp/                    Ethereum's Recursive-Length-Prefix codec explained
    seed-phrases/           How seed phrase recovery works
    sk-awcp/                How to use Sidekick and AWCP
jrx/                    [TS] FLAGSHIP: the Jack Russell browser wallet extension
libs/                   Miscellaneous (non-flagship) libraries
    awcp/                   [TS]  Aepp-Waellet Communication Protocol: types for messages sent between page scripts and browser extension wallets
    parasite/               [TS]  Function library for talking to public aeternity nodes (do not use please)
    tweetnacl/              [TS]  Jex-packaged clone of public domain TweetNaCL library
    vdk_aeser/              [TS]  Serializer/deserializer library for AE data structures
    vdk_base58/             [TS]  Base 58 encode/decode library
    vdk_base64/             [TS]  Base 64 encode/decode library
    vdk_binary/             [TS]  Miscellaneous binary functions that should exist in the stdlib but don't
    vdk_colors/             [TS]  Assigns a color to arbitrary byte array (for color-coding keys in JR)
    vdk_faert/              [TS]  Fast AEternity Recovery Text: a replacement for the Bitcoin seed phrase standard
    vdk_names/              [TS]  Assigns a human readable name to a byte array (for naming keys in JR)
    vdk_rlp/                [TS]  Ethereum RLP standard TypeScript library
    vdk_safe/               [TS]  Equivalent of Haskell's Either type
    vdk_tests/              [TS]  Test/example suite for VDK
    vdk_tests_cases/        [TS]  Randomly generated test cases for some VDK packages (e.g. checking our RLP agrees with Ethereum's)
    vrlp/                   [ERL] Ethereum RLP library in Erlang
sidekick/               [TS] FLAGSHIP: TS library to talk to browser wallet extension (e.g. Superhero, hence name) from page script
sidekick_examples/      [TS] Example/test suite for sidekick
site/                   Website for the vanillae project (soon to be deprecated/deleted)
utils/                  Miscellaneous utilities
    jex/                    [ERL] Simple TS/JS packaging utility, our alternative to NPM
    vlogd/                  [ERL] Simple HTTP server that just logs input data, for demoing sidekick's HTTP logging functionality
    vw/                     [ERL] Vanillae Wallet: scratchpad for working out complicated wallet stuff (e.g. seed phrase recovery) in saner environment
```

## Flagship products

### Vanillae Bindings

### Jack Russell

### Sidekick

## Miscellaneous products of note

### Vanillae Files

### Jex

[aegora]:           https://aegora.jp
[aesdk]:            https://github.com/aeternity/aepp-sdk-js
[grant-proposal]:   https://forum.aeternity.com/t/active-application-vanillae/10638
[vanillae-files]:   ./docs/
[zx-zomp]:          http://zxq9.com/projects/zomp/
