# awcp: Aepp-Waellet Communication Protocol.

Mostly an internal package.

Page scripts (aepps) and wallet browser extensions (waellets) need to talk to
each other.  This package contains

1. type definitions of the messages
2. a list of error codes

[src/awcp.ts](./src/awcp.ts) contains a detailed explanation of how the
protocol is structured.

[Sidekick](../../sidekick/) exhibits how to use this from the perspective of
the page script (aepp), and [Jack Russell](../../jrx) exhibits how to use this
from the perspective of the waellet.

## Building

Requires [jex]

```
jex install
```

[jex]: ../../utils/jex/
