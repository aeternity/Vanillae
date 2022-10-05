# Sidekick

Sidekick is a simple JavaScript library for talking to an Aeternity
browser wallet extension such as Jaeck Russell or Superhero from the document
context of a webpage.

> One of the most important things for designing a computer, which I
> think most designers don't do, is you study the problem you want to
> solve. And then use what you learn from studying the problem you
> want to solve to put in the mechanisms needed to solve it in the
> computer you're building. No more, no less.

— Gerald Jay Sussman



# Build Prereqs

Assuming Ubuntu 18.04. Adapt these instructions for your own system.

You need

- `npm` to build TypeScript
- `tsc` to compile
- [`jx`][jx] to orchestrate the build properly
- Python 3.6 or later to run `jx`

[jx]: https://gitlab.com/pharpend/jx/-/tree/master/#jx-secure-typescript-package-manager

Steps

    sudo snap refresh
    sudo snap install node --channel 18/stable
    npm install -g typescript
    wget https://gitlab.com/pharpend/jx/-/raw/master/jx -O ~/.local/bin/jx
    chmod u+x ~/.local/bin/jx



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



# Build

    make


# Examples

There is a repository of examples at https://gitlab.com/pharpend/sidekick_examples.git

# What sidekick is NOT

Sidekick is **not** ideal if you operate in the Node/NPM ecosystem.  If you are
working in the Node/NPM ecosystem, you probably want the [Aeternity JavaScript
SDK](https://github.com/aeternity/aepp-sdk-js/).  Every single
Aeternity-related thing you could ever possibly want to do is possible to
accomplish with the SDK.

All that Sidekick knows how to do is talk to a browser wallet extension.  In an
application, there is a great deal of necessary functionality (e.g. forming
transactions for the wallet to sign) which sidekick assumes your server-side
code has already handled.

In all software there is a tradeoff between simplicity and number of features.
Sidekick is very simple: 2300 lines of TypeScript, including comments, with no
dependencies.  Therefore Sidekick intentionally only has a very limited set of
features.



# Where is the NPM package or the webpack bundle?

There isn't one.

## Why?

Sidekick is a library that deals with cryptocurrency. The security
model is based on transparency and trust.  A user must be able to
inspect code that is running on his hardware handling his money.

We don't support NPM because of the security issues that NPM
introduces.  Briefly, the code can change at any time under the
developer's nose without the developer knowing.  The [leftpad
debacle][lpad] and the [RIAEvangelist debacle][ria] are good examples
of the types of vulnerabilities that package managers like NPM
enable.

[lpad]: https://archive.ph/Qsh7j
[ria]: https://archive.ph/OF5I9

We don't use something like webpack because that introduces an opaque
rewrite using an untrusted tool.  Webpack could plausibly alter the
runtime behavior of the program, and we would have no way to detect
that.  Even if we trusted webpack, how do we obtain webpack?  NPM.
So.

It is debatable whether or not we should trust the TypeScript
compiler (TSC).  On the whole, TSC probably makes Sidekick more
secure, simply by virtue of increasing overall code quality and
eliminating the largest categories of potential bugs.  Moreover, the
output that TSC produces is human-readable, and has a very
straightforward mapping to the original source code.  A human can
easily read the TSC-generated JavaScript tree, even without the aid
of the source map, and have a high degree of faith that the code is
trustworthy and that TSC is behaving as promised.




Source: https://github.com/sindresorhus/guides/blob/main/npm-global-without-sudo.md


# How to build and view documentation

```
make build_docs
make serve_docs
```


