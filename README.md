# semux-discord

![semux-light](https://github.com/witoldsz/semux-discord/raw/master/semux-discord-bot.png)

## Prerequisites

This app is written in Haskell and built using `stack`. Just install _The Haskell Tool Stack_ and you should be able to build and run with no issues.

## Build and run it like this:

`./env.sh`
```sh
#!/bin/sh
export SEMUX_API="https://api.semux.online/v2.1.0/"
export DISCORD_SECRET=your_secret
```

`./repl.sh`
```sh
#!/bin/sh
. ./env.sh
stack repl
```

`./start.sh`
```sh
#!/bin/sh
. ./env.sh
stack run semux-discord-exe
```
