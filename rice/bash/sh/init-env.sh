### init-env.sh -- Environment variables

### XDG
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

### Input method
export GTK_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"
export XMODIFIERS="@im=fcitx"

### PATH
PATH="$PATH:$(dirname $ribash_home)/bin"

### Common
export EDITOR="vim"

### Authentication agents
update-ssh-agent() {
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
    if [[ ! -e $SSH_AUTH_SOCK ]]; then
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
    fi
}

update-gpg-agent() {
    unset SSH_AGENT_PID
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    export GPG_TTY="$(tty)"
    gpg-connect-agent updatestartuptty /bye > /dev/null
}
