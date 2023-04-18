### alias.bash -- Bash aliases

### Authentication agents
reload-ssh-agent() {
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
    if [[ -e $SSH_AUTH_SOCK ]]; then
        return
    fi
    eval $(ssh-agent -a $SSH_AUTH_SOCK)
}

reload-gpg-agent() {
    if [[ "x$1" == "x-f" ]]; then
        echo "Restarting gpg-agent..."
        gpgconf --kill gpg-agent
    fi
    unset SSH_AGENT_PID
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    export GPG_TTY="$(tty)"
    gpg-connect-agent updatestartuptty /bye > /dev/null
}

### linuxbrew
brewenv() {
    eval "$(brew shellenv)"
    export PS1="(brew) $PS1"
}

### Usage: srting-join delimiter string1 string2 ...
string-join() {
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}
