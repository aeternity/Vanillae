# Sidekick

Sidekick is a simple JavaScript library for talking to an Aeternity
browser wallet extension such as Jaeck Russell or Superhero from the document
context of a webpage.

# Build Prereqs

Assuming Ubuntu 18.04. Adapt these instructions for your own system.

You need

- `npm` to build TypeScript (and TypeDoc if you want to build the
  documentation)
- `tsc` to compile
- [jex](../utils/jex/) to facilitate the build

Steps:

    sudo snap refresh
    sudo snap install node --channel 18/stable
    npm install -g typescript

## Protip: avoid using `sudo npm install -g`

If you want to avoid using sudo

```
mkdir ~/.npm-packages
npm config set prefix "${HOME}/.npm-packages"
```

Edit `~/.bashrc` or `~/.zshrc` with

```
NPM_PACKAGES="${HOME}/.npm-packages"
export PATH=$NPM_PACKAGES/bin:$PATH
```

# Build Steps

## 1. Build dependencies

The examples require `parasite` as a dependency, but sidekick itself does not.

```
~/src/vanillae $ cd libs/awcp
~/src/vanillae/libs/awcp $ jex dwim+
~/src/vanillae/libs/awcp $ cd ../parasite
~/src/vanillae/libs/parasite $ jex dwim+
```

## 2. Build sidekick

```
~/src/vanillae/libs/parasite $ cd ../../sidekick
~/src/vanillae/sidekick $ jex dwim+
```

## 3. Build examples

Note the `-`, not the `+`. The difference is that `-` just builds the project,
but does not package it.

```
~/src/vanillae/sidekick $ cd examples
~/src/vanillae/sidekick/examples $ jex dwim-
~/src/vanillae/sidekick/examples $ python3 -m http.server 8000
```

Navigate to `http://localhost:8000/` in your browser to see the examples

The examples are the best documentation, for now.
