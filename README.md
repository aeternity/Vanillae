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

Vanillae grew out of [Aegora.jp](https://aegora.jp).  We made a simple
e-commerce store using Aeternity as the payment system.  We encountered a lot
of rough edges in Aeternity's tooling, and so developed our own, and
open-sourced it.  That became this project

Our tooling is grouped according to the project that spawned it

-   [Sidekick](./sidekick/)

    We needed the ability to talk to Superhero (the standard Aeternity browser
    wallet extension) from our page script. Sidekick is a simple library to do
    just that.

    -   [Sidekick Examples](./sidekick_examples/) illustrate how to use sidekick

    -   [AWCP](./libs/awcp/): the aepp/waellet communication protocol

        This is the definition of the messaging protocol between the page
        script and the wallet.  Sidekick implements one half of this protocol.

    -   [jex](./utils/jex/)

        We encountered packaging as an obvious problem to solve, and NPM is
        simply a terrible way to solve it. Jex is a TypeScript/JavaScript
        package manager that avoids most of the NPM-related security issues.

-   [Vanillae Erlang Bindings](./bindings/erlang)

    Our server backend is written in Erlang.  We needed to talk to the
    blockchain from our server backend.  Vanillae.erl is an Erlang application
    that does that.

    It is designed in such a way that you could write say a Vanillae Go
    library with an identical API but totally different internals.  Ultimately
    the idea here is to create a language-agnostic API for talking to the
    Aeternity blockchain from the

-   [Jaeck Russell](./jrx/): this is a (work-in-progress) simpler wallet than
    Superhero

-   [Vanillae Files](./docs/): any time where we encountered some weird thing
    that is like 1 hour of information but takes 15 hours to understand because
    it's poorly documented, we tried to document it here.
