#!/bin/sh

if [ "--kill" = "$1" ]; then
    gpgconf --kill gpg-agent
    echo "Killed gpg-agent"
    exit 0
fi

env GPG_TTY="$(tty)" gpg-connect-agent updatestartuptty /bye >/dev/null
