### init-alias.sh -- Aliases for applications

### Common aliases
## ls
alias ll="ls -lahF --color=auto"

### Emacs
## Launch a client in current terminal and start server if it hasn't
alias em="emacsclient -c -nw -a="
## Launch a client in a frame and start server if it hasn't
alias emm="emacsclient -c -a="
## Open files quickly
alias emq="emacs -Q -nw"
## Dired
emf() { emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \"$(pwd)\"))"; }
## Quick evaluation
eme() { emacs -Q --batch --eval "(message \"%s\" $@)"; }
## Daemon
emdaemon() { emacsclient -e 't' &>/dev/null || emacs --daemon; }

### Authentication agents
reload-ssh-agent() {
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
    if [[ -e $SSH_AUTH_SOCK ]]; then
        return
    fi
    eval $(ssh-agent -a $SSH_AUTH_SOCK)
}

reload-gpg-agent() {
    if [[ "x$1" = "x-f" ]]; then
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
    alias brew=/home/linuxbrew/.linuxbrew/bin/brew
    eval "$(brew shellenv)"
    export PS1="(brew) $PS1"
}

brew() {
    env HOMEBREW_NO_AUTO_UPDATE=1 PATH=/home/linuxbrew/.linuxbrew/bin:$PATH /home/linuxbrew/.linuxbrew/bin/brew $@
}
