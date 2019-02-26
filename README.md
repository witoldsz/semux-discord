# semux-discord

## Prerequisites

This app is written in Haskell and built using `stack`. Just install _The Haskell Tool Stack_ and you should be able to build and run with no issues.

## Build and run it like this:

`./start.sh`
```sh
#!/bin/sh
stack build
SEMUX_API="https://api.semux.online/v2.1.0/" \
ALERT_AFTER_SECS=3600 \
DELEGATE="…delegate address" \
WEBHOOK_URL="…discord webhook url" \
.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/semux-discord-exe/semux-discord-exe
```

Replace `dist/x86_64-linux/Cabal-2.4.0.1` appropriately.
