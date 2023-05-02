#!/bin/sh

## From: https://kevinlocke.name/bits/2019/07/31/prefer-terminal-for-gpg-pinentry/
## Use this in conjunction with reload-gpg-agent.sh

## Set environment variable PINENTRY_USE_DATA to determine which pinentry to use
case "${PINENTRY_USER_DATA}" in
    *USE_TTY*)
        ## Use prompt in the terminal
        ## Note: Change to pinentry-curses if a Curses UI is preferred.
        exec pinentry-tty "$@"
        ;;
    *)
        ## Otherwise, use a UI dialog
        ## Note: Will fall back to curses if $DISPLAY is not available.
        exec pinentry-qt "$@"
        ;;
esac
