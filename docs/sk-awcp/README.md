# Sidekick, AWCP, and Jex

## Contents

## Background and Motivation

Sidekick is a very simple TypeScript/JavaScript library for talking to an
Aeternity browser wallet extension (e.g. Superhero or Jack Russell) from a page
script.

AWCP (Æpp-Wællet Communication Protocol) is the messaging protocol between an
extension and a wallet. Sidekick implements this from the perspective of a page
script (the "Æpp").

Jex is my homemade alternative to NPM.  It is currently super confusing and
unusable, but I maintain it is significantly better than NPM.

The motivation was [Aegora.jp](https://aegora.jp/), which is a basic e-commerce
site where items are priced in Aeternity cryptocurrency.

We (Craig and Peter) started working on Aegora around January of 2022, and
launched on December 8, 2022.

Our first task was quite simple: discover if the user has a wallet installed.
If so, make the page background green; if not, make it red.

This seemingly simple task was absurdly difficult with the Aeternity JavaScript
SDK, at least in its state as of early 2022. The SDK has been improved
significantly since that time, but it is still nowhere near production quality.
To this day, I would have no idea how to do that task.

Here is how to do it with Sidekick:

```ts
// from https://github.com/aeternity/Vanillae/blob/892c9cd9da5dcbf50224a26b8da9ab2fd38435ab/sidekick/examples/src/connection.ts#L34-L66https://github.com/aeternity/Vanillae/blob/892c9cd9da5dcbf50224a26b8da9ab2fd38435ab/sidekick/examples/src/connection.ts#L34-L66
let maybe_wallet = await sk.detect(sk.TIMEOUT_DEF_DETECT_MS,       // timeout
                                   "failed to detect wallet",      // error if timeout
                                   logger);                        // logger
```

You have to pass in a logger to each call.  You probably want `sk.cl()` which
is the console logger.  Sidekick exports a few other loggers, as well as the
`sk.Logger` interface if you want to write a custom one. For instance, maybe
you want to log the page script/wallet conversation on your own server, so if
something goes wrong, you hear about it. You can write your own logger which
`POST`s the logs back to your own server, and just pass it in as an argument.

Anyway, back in Februaryish of 2022, I ended up going to work trying to figure
out how the SDK was communicating with Superhero.  It turns out there's an
event bus available to both the document and extension context, and posting
messages in said event bus is how the two communicate.

Sidekick started at this time as just a scratch project to figure out what the
SDK was actually doing.  I (Peter) wrote it originally as a fuzzer that
replayed the SDK's half of the conversation, with no intention of ever using it
in production.

At some point, it dawned on us that I had written a 2000 line TypeScript
library that did exactly what we needed for Aegora purposes, that we understood
perfectly, was easy to use, and was entirely self-contained (i.e. no external
dependencies, could be wrapped up in a tarball and dropped on the server). And
maybe we should just use that.  So we did.

By the way, the tooling to do "just wrap it up in a tarball and drop it on a
server" is Jex.

## Annotated example: Aegora log-in code

Background: we don't use passwords to do login at Aegora (we don't store *any*
private information about our users).  Instead, we have your wallet
cryptographically sign a random message, and then verify the signature.

Here is the page script for the Aegora log in page, annotated.
You can follow along reading <https://aegora.jp/r/115/sign_mess-2.js>.

The first thing we do is import sidekick

```js
import * as sk from "./sidekick-0.2.0/dist/sidekick.js";
```

The idea here is you can just unpack the tarball on your server and just use
it. Everything is versioned so you can take advantage of caching.

Here is the super complicated `main` function:

```js
function main()
{
    let logger = sk.cl();
    detect(logger);
}
```

Let's look at the `detect` function.

```js
async function detect(logger)
{
    // try to detect the wallet
    let detective = await sk.detect(sk.TIMEOUT_DEF_DETECT_MS, "no waellet", logger);
    // if the wallet was detected, then proceed to the next step
    if (detective.ok)
    {
        connect(logger);
    }
    // otherwise show an error message
    else
    {
        console.log(detective);
        let failure = document.getElementById('failure');
        searching.style.display = "none";
        searching.style.visibility = "hidden";
        failure.style.display = "block";
        failure.style.visibility = "visible";
    }
}
```

That's basically the entire pattern. There's a sequence of steps to talk to the wallet:

1.  `detect`
2.  `connect`
3.  `address`
4.  You've now established a conversation with the wallet, and you now
    do one of two things:

    1.  Ask the wallet to sign a transaction: `tx_sign_noprop`.
    2.  Ask the wallet to sign a message: `msg_sign`.

        This is what we're going to do here shortly

Every "porcelain" call in sidekick returns this `Safe` type:

```ts
type Ok<ok_t>
    = {ok     : true,
       result : ok_t};

type Error<err_t>
    = {ok    : false,
       error : err_t};

type Safe<ok_t, err_t>
    = Ok<ok_t>
    | Error<err_t>;
```

This allows you to use an `if` statement to ask the simple question "did it
work or not"?  The pattern here is:

1. Perform a step
2. Use an `if` statement to tell if it worked or not. (The user rejecting a request from the page counts as "not working").
3. If it worked, proceed to the next step
4. If it didn't, show an error message

You might want to re-read the `detect` function, and then let's continue with `connect`:


```js
async function connect(logger)
{
    let maybe_wallet = await sk.connect('ske-connect-1',                // arbitrary string|number. for sorting out "this message from the wallet is the response to this prior message from the page script"
                                        {name: 'Aegora', version: 1},   // message we're sending to the wallet. name is arbitrary, version must be 1
                                        2000,                           // timeout (in milliseconds); this call is instantaneous in practice
                                        "failed to connect to wallet",  // error on timeout
                                        logger);
    let searching = document.getElementById('searching');
    // if it worked, proceed to read_key
    if (maybe_wallet.ok)
    {
        searching.style.display = "none";
        searching.style.visibility = "hidden";
        let found = document.getElementById('sign_dis_chit');
        found.style.display = "block";
        found.style.visibility = "visible";
        read_key(logger);
    }
    // otherwise show an error message
    else
    {
        console.log(maybe_wallet);
        let failure = document.getElementById('failure');
        searching.style.display = "none";
        searching.style.visibility = "hidden";
        failure.style.display = "block";
        failure.style.visibility = "visible";
    }
}
```

Get the picture? Alright, let's look at `read_key`:

```js
async function read_key(logger)
{
    // get the user's public key
    let wallet_info = await sk.address('ske-address-1',                         // coordination id
                                       {type: 'subscribe', value: 'connected'}, // message you're actually sending to the wallet, must be exactly this
                                       300000,                                  // timeout (this pops up a confirm dialog for the user, so you need to give the user time to read it)
                                       "failed to address to wallet",           // error on timeout
                                       logger);
    // if it worked, dig out the key and proceed
    if (wallet_info.ok)
    {
        // this call returns this ridiculous data structure where the user's
        // public keys are the keys in a hashmap that point to empty objects
        //
        // it's literally this:
        //
        //    {ok     : true,
        //     result : {subscription : ["connected"],
        //               address      : {current   : {"ak_2XhCkjzTwcq1coXSSzHJoMZkUzTwnjH88zmPGkkowUsFNTo9UE": {}},
        //                               connected : {"ak_21HW2BeR8KQnzB76b9RSeNAXFf8SEvquLG3ichyLaXhdxUpXe9" : {},
        //                                            "ak_Bd9rA8pDWucwfriVp6Zgb68csxanCzWDqstyoBKBbzUnNhpKQ"  : {},
        //                                            "ak_TuwioiZCt3Ajx9dgVS9qdnS9VW1t4GMWFML5zBPgzouZUGUDA"  : {},
        //                                            "ak_ywR1N7GDpj7djeEEEnHSTmYbQmxvWCvFgfsLpxdFpK1ptkZMU"  : {}}}}}
        //
        // so you need this beautiful line of code to actually dig out the key:
        let pk = Object.keys(wallet_info.result.address.current)[0];
        // proceed
        sign_mess(pk, logger);
    }
    // otherwise, error
    else
    {
        console.log(wallet_info);
        let failure = document.getElementById('failure');
        failure.style.display = "block";
        failure.style.visibility = "visible";
    }
}
```

Alright, let's look at actually signing the message:

```js
async function sign_mess(pk, logger)
{
    let public_key = document.getElementById('public_key');
    public_key.value = pk;
    // this field has the message we're going to have the user sign
    let blob = document.getElementById('unsigned').value;
    // ask the wallet to sign it
    let signature = await sk.msg_sign('ske-msg_sign-1',
                                      pk,
                                      blob,
                                      sk.TIMEOUT_DEF_MSG_SIGN_MS,
                                      'message signing took too long',
                                      logger);
    // if it worked, populate the form fields and make the form submittable
    if (signature.ok)
    {
        let signed_data = signature.result.signature;
        let signed = document.getElementById('signed');
        signed.value = signature.result.signature;
        // oh that's right
        // forgot about this
        // for some reason, javascript indexes timezones by negative minutes
        // everything in this language is like this
        let ts = document.getElementById('ts');
        ts.value = new Date().getTimezoneOffset() * -60;
        let submit_button = document.getElementById('submit_button');
        submit_button.disabled = false;
    }
    // otherwise, show the error in the signature field
    else
    {
        console.log(signature);
        let signed = document.getElementById('signed');
        signed.value = signature;
    }
}
```
