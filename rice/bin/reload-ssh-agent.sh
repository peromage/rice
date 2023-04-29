#!/bin/sh

if [ -z "$SSH_AUTH_SOCK" ]; then
    echo 'echo "$SSH_AUTH_SOCK is not set"'
    exit 1
fi

if [ "--kill" = "$1" ]; then
    ## Get $SSH_AGENT_PID variable
    eval "$(ssh-agent -a "$SSH_AUTH_SOCK")"
    ssh-agent -k
    echo 'echo "Killed ssh-agent"'
    exit 0
fi

ssh-agent -a "$SSH_AUTH_SOCK"
