### init-default.sh -- Default bash settings

### Environment variables
export PATH="$PATH:$(dirname $ribash_home)/bin"
export EDITOR="vim"

### Authentication agents
update-ssh-agent() {
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
    if [ ! -e $SSH_AUTH_SOCK ]; then
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
    fi
}

update-gpg-agent() {
    unset SSH_AGENT_PID
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    export GPG_TTY="$(tty)"
    gpg-connect-agent updatestartuptty /bye > /dev/null
}
