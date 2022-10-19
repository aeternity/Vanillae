# Vanillae

Vanillae is an Aeternity tool suite meant for small business use cases.  Focus
is placed on ease-of-use, simplicity, code quality, and good documentation.

You can read the [grant
proposal](https://forum.aeternity.com/t/active-application-vanillae/10638) for
more information about what this project is.

Vanillae is (intentionally) a very minimal toolset that only covers the most
common Aeternity use cases.  We also do not have any tooling for the Node/NPM
ecosystem. If you have a more complicated use case or live in the Node/NPM
ecosystem, you want the [Aeternity JavaScript
SDK](https://github.com/aeternity/aepp-sdk-js). The SDK is a Swiss Army Knife.
Vanillae is just a knife.

## Flagship Tools

-   Jaeck Russell

    A minimal browser wallet extension. Not included in this repository yet.

-   [Sidekick](./sidekick/)

    A library to talk to a browser wallet extension from the perspective of a
    page script.

-   Bindings

    These are language-specific libraries that expose the most common functions you
    need in a backend application that uses Aeternity.

    These intentionally do not cover obscure or esoteric Aeternity features
    (e.g. hyperchains).

    - [Erlang](./bindings/erlang/)


## Libraries

These are libraries that were developed incidentally which made sense to factor
out.

-   [AWCP](./libs/awcp/)

    Aepp/waellet communication protocol

    This is just type definitions and constants (error codes). Type definitions
    consist of what data the waellet expects from the aepp, and what data the
    aepp expects from the waellet.

-   [Parasite](./libs/parasite/)

    **NOT FOR PRODUCTION USE**

    This is a library for talking to Aeternity HTTP nodes from the perspective
    of a page script.  Used in example/documentation code for things that your
    backend should do.

    This may eventually be polished and repackaged as a production quality
    library.

-   libjr

    This is a library to talk to a page script ("aepp") from the perspective of
    the wallet ("waellet"). It essentially is sidekick from the perspective of
    the wallet.

    Not yet included in this repository.


## Utilities

These are applications that were developed incidentally during the course of
this project.

-   [jex](./utils/jex/)

    A TypeScript/JavaScript package manager that avoids a large class of
    NPM-related security issues.

-   [vlogd](./utils/vlogd/)

    **NOT FOR PRODUCTION USAGE**

    Simple HTTP server that just logs requests it gets. Used in example code
    for demonstrating sidekick's logging capabilities.

## Documentation

-   [base64 versus base58 explainer](./docs/baseN/)

    These are two numerical notations used in the Aeternity project.

-   [NPM guide](./docs/npm-misc/)
