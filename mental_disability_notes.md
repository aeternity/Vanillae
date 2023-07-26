# NACL mental disability

NACL is mentally disabled and can't be loaded as an ES6 module. That is, it can't be
"imported" using

```js
import * as nacl from './path/to/nacl.js';
```

Instead you have to include it in a page. So we have a page called
`background.html`.

I naively tried this

```html
<script src="dist/jex_include/local-nacl-1.0.3/nacl.js"></script>
```

That of course didn't work because we live in hell.

I initially thought the problem was an invalid hash. No. Notes as I figure it
out:


    Trying to figure out why TweetNaCl isn't loading

    ok so the problem is not an invalid hash, it's something specific
    to nacl

    to have an inline script, its hash must be specified in
    manifest.json

    example (integrity key is not necessary, it is there purely for
    comment purposes)

         beginning of line
        V
        |        <script integrity="sha256-8T3dvQxHPNQvgxzCemw3KGUCM5RhknpCQF6pCcTdFTw=">console.log('bg-test-inline.js line 1!');
        |</script>

    newline is there for hash purposes

    this works if and only if
    'sha256-8T3dvQxHPNQvgxzCemw3KGUCM5RhknpCQF6pCcTdFTw=' is included
    in the manifest.json

    example (top level key):

         "content_security_policy" : "script-src 'self' 'sha256-8T3dvQxHPNQvgxzCemw3KGUCM5RhknpCQF6pCcTdFTw='; object-src 'self'",

    Absent that explicit allow, I get this error from Firefox:

        Content Security Policy: The page’s settings blocked the loading of a resource at inline (“script-src”).

    Refs:
        1. https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/content_security_policy
        2. https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Content_Security_Policy#inline_javascript

    If I include the same script as a file,

        <script src="bg-test-inline.js"></script>

    this works irrespective of if I specify it as allowable in the manifest

    nacl is just refusing to load

        Loading failed for the <script> with source “moz-extension://eafd6a74-b75b-4312-9a34-8087b68395e7/dist/jex_include/local-nacl-1.0.3/nacl.js”.

    I initially thought it was a hash problem, but it is not

    TweetNaCL doesn't work as an ES6 module, it works by destructively
    updating the window namespace.  This I think was necessary before
    ES6, and that behavior is retained either because of laziness or
    because of backward compatibility reasons.

    hmm

    so this just plainly doesn't work and i cannot figure out why

        <script src="dist/jex_include/local-nacl-1.0.3/nacl.js"></script>

I think it works if I do it in the popup window instead. Let's try

oh no

no

oh my god

i am so stupid

the problem was that i typed the url wrong

all this time i thought that it was the browser that was mentally disabled

it turned out the mental disability was inside me all along

alright well I discovered a potential future pitfall at least
