# Vanillae

Vanillae is an Aeternity tool suite meant for small business use cases.  Focus
is placed on simplicity, code quality, and good documentation.

## About

### Flagships Applications

- Jaeck Russell (Erlang)

  A minimal browser wallet extension.

- jex (Erlang)

  A TypeScript/JavaScript package manager that avoids a large class of
  NPM-related security issues.

- vlogd (Erlang)

  **NOT FOR PRODUCTION USAGE**

  Simple HTTP server that just logs requests it gets.


### Erlang Libraries

- vanillae


### TypeScript Libraries

- awcp

  Aepp/waellet communication protocol

  This is just type definitions, what data the wallet expects from the Aepp,
  and what data the aepp expects from the waellet.

- libjr

  This is the complement to sidekick. It essentially is sidekick from the
  perspective of the waellet.

- sidekick

  This a TypeScript/JavaScript library for talking to Aeternity browser wallets
  (e.g. Superhero, Jaeck Russell) from the perspective of an "aepp" (i.e. your
  website's JavaScript).


## Documentation

- base64 versus base58 explainer
