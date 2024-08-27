#!/usr/bin/env bash

## Set environment variable PINENTRY_USE_DATA to determine which pinentry to use
## Ref: https://kevinlocke.name/bits/2019/07/31/prefer-terminal-for-gpg-pinentry/

## NOTE: Use this in conjunction with reload-gpg-agent.sh


## Use prompt in the terminal
## Note: Change to pinentry-curses if a Curses UI is preferred.
[[ "${PINENTRY_USER_DATA}" == *USE_TTY* ]] && exec pinentry "$@"

## Otherwise, use a UI dialog
## NOTE: Will fall back to curses if $DISPLAY is not available.

## A list of pinentry executables
## NOTE: The order matters
PROGS=(pinentry-gnome3 pinentry-gtk-2 pinentry-qt pinentry-curses pinentry)
executable=

for i in "${PROGS[@]}"; do
    executable="$(command -v $i)" && break
done

if [[ -z "$executable" ]]; then
    echo "No pinentry found" >&2
    exit 1
fi

exec "$executable" "$@"
