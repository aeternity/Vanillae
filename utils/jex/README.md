# Jex: simple TypeScript/JavaScript packaging system

Jex is a work-in-progress package manager developed to solve the specific
packaging needs of the vanillae project.

## Installation

Jex is very much a work in progress, so these instructions are subject to
change, and may not work anymore by the time you are reading this

0.  Install system dependencies
    ```
    tree rsync
    ```
1.  `git clone https://github.com/aeternity/Vanillae.git`
2.  [Install Erlang and zx](https://www.bitchute.com/video/1gCvcoPUR7eJ/)
3.  Install NPM ["How to install NPM without getting AIDS"](../../docs/npm-misc/README.md)
4.  Install TypeScript `npm install -g typescript`
5.  Edit `~/.bashrc` (or `~/.zshrc` or whatever) and add

    ```
    alias jex="zx rundir ~/path/to/Vanillae/utils/jex"
    ```

## Usage

```
Jex: simple JavaScript packaging system

COMMANDS:
  man             show the manual
  dwim-           init, pull, build
  dwim+           init, pull, build, mindist, push
  cfgbarf         barf out the jex.eterms file (mostly to make sure it parses correctly)
  echo home       echo $HOME
  echo jexdir     echo $HOME/.jex
  echo devdir     echo $HOME/.jex/dev
  echo pkgname    name of current package
  echo pkgdir     echo $HOME/.jex/dev/realm-name-X.Y.Z
  echo deps       list dependencies of current package
  echo pathof PKG list the path to PKG or 
  init            mkdir -p $HOME/.jex/dev
  build           tsc && cp -r ./src/jex_include ./dist/
      -w, --weak      continue building even if tsc fails
      -f, --force     use cp -rf instead of cp -r
  mindist         mkdir jex_mindist && cp -r src jex_mindist && cp -r dist jex_mindist && rm -r jex_mindist/src/jex_include
      -f, --force     use cp -rf instead of cp -r
  push            rsync -a jex_mindist/ PKGDIR
  ls              ls $HOME/.jex/dev
  tree            tree $HOME/.jex/
  rmpkg PKG       rm -r $HOME/.jex/dev/PKG
  pull            pull each dependency into src/jx_include
```

## About

As of now, Jex is a glorified shell script that automates a lot of the tedium
in building sidekick, JR, etc.

The long-term goal is to completely remove any dependency on NPM.  NPM comes
with a lot of unfixable security issues that present an unacceptable risk in a
business context, which is the focus of the Vanillae project.

This isn't something like yarn where it's the same thing as NPM but it is
prettier or something. Totally different packaging concept.

### Differences from NPM


1.  **Minimizing dynamicism**

    The basic assumption of NPM is that everything works all the time, and
    because that isn't true, nothing ever works and everything is always
    broken.  The basic assumption of Jex is that nothing ever works and
    everything is always broken, and because that's true, everything works all
    the time, sometimes.

    Concretely, this means that you have to do all dependency management
    manually. If you are trying to build package `A` and it depends on packages
    `B`, `C`, and `D`, then you have to go find packages `B`, `C`, and `D`,
    build them and then go back and build package `A`.

    Again, the assumption (objectively true) is that everything is always
    broken.  So on the off chance that something works by happenstance, Jex's
    strategy is to give it the Ted Williams treatment and never touch it ever
    again.

    There's an implicit assumption here that the total volume of JavaScript
    code is fairly small.  Manual dependency management is not feasible if your
    project has 11,000 dependencies, requires 18 different packages called
    "babel" just to build, and "minifies" to a 20kb opaque blob.  As a
    guideline, if the number of external package dependencies grows large
    enough that a single developer cannot manage them manually, then, (over
    time) the primary activity of said developer becomes trying to get all the
    different packages to play nicely together.

    In other words, manual dependency management is inescapable.  Jex is simply
    honest about that fact and prevents you from digging yourself into a hole.

2.  **Node is bad**

    The next departure from NPM is that we don't care at all about the node
    runtime.  JavaScript was invented by Satan as a joke.  Using JavaScript at
    all for any reason is a terrible idea.  It is an especially terrible idea
    to write any code in JavaScript that does not absolutely have to be written
    in JavaScript.

    All that is to say, whenever we write JavaScript code, the assumption is
    that the code will be run in a browser and only in a browser.

    We are never writing generic libraries that could either run in the browser
    or run in the node runtime.
