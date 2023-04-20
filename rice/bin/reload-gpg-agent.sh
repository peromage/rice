#!/bin/sh

if [ "x--kill" = "x$1" ]; then
    gpgconf --kill gpg-agent
    echo "Killed gpg-agent"
    exit 0
fi

gpg-connect-agent updatestartuptty /bye > /dev/null
