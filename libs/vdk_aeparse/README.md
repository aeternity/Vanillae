# `vdk_aeparse`

Parsing/validation for AE data.

So this is kind of a big library that does a lot of things.  The reason is that
as an experiment I am giving [zod][z] a shot.  I really really like the idea of
Zod, and it's genuinely extremely well-done, but I really really dislike
external dependencies.

I especially dislike the sort of external dependencies like zod where I am
encouraged to write all of my code against their API.  This invites AWS
problems, where I end up writing my entire codebase against a specific API
that I don't control.  If you want to get rid of it, it's super difficult, and
realistically it's a rewrite.

This differs categorically from small self-contained libraries like NaCl or
whatever where the code

1. very rarely changes
2. only manifests in a small number of function calls that only appear a handful of places

That is, if I ever decide I don't like NaCl, I can always just write my own
replacement, and it's not major surgery to my codebase to switch to a new
implementation.

Zod infects everything.

So this is basically a quarantine package...

hmmm... maybe I should put all of my code that has external dependencies here.

Hmm... can't do that with tweetnacl because it doesn't like the modern js
module system.

But hmm... thoughts to think about

this should be named like the pitri dish or something.

Anyway, then you have the additional problem of what happens when they decide
to push breaking changes.  This is a problem with every external package, but
*especially* with those that are actively maintained like zod. That's always a
headache.  If you have like 5 external dependencies that are actively
maintained projects, very quickly your job becomes desperately trying to get
them all to work together.  An average JS project has like 500.  I am just
really trying hard to avoid this idea altogether, to the point of writing my
own package manager.

I do have to capitulate *somewhat* to the everything-is-AIDS problem. Like I
use TypeScript for instance. TypeScript is a project of Microsoft. Microsoft is
of course a reputable company with no history whatsoever of making people
depend on their products and then sabotaging them in order to hinder their
competition, so don't really need to worry in this case.

But, similar to Zod, TypeScript is extraordinarily well-done.  One of my
colleagues said "TypeScript is a latex glove when you need to build a statue
out of shit."  Perfect description.

Certain individuals I talk to get caught up on the context of building statues
out of shit, and think (erroneously) that it is a waste of brainpower to spend
$1 billion on fecal sculpture tooling, and fail to see that the latex glove is
a monumental technical achievement that has obvious application outside the
practice of fecal sculpture.

TypeScript, I can rationalize away also because it produces plain JS.  Even at
that point I have to trust the JS interpreter to behave honestly.  But I have
no choice but to trust such things.

Anyway, *originally* this was serialization/deserialization routines for AE
data. For instance, read a `tx_...` string and figure out what the sender,
recipient, payload, etc are

[z]: https://github.com/colinhacks/zod/blob/481c9ba1932203777f6fe9497bb2a8a1d33c620e/README.md#basic-usage


## building

requires [jex](../../utils/jex/)

```
jex install
```
