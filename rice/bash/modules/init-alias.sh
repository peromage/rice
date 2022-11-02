### init-alias.sh -- Aliases for applications

### Common aliases
## ls
alias ll="ls -lahF --color=auto"

### Emacs
## Open files in the terminal
alias em="emacsclient -c -nw"
## Open files in the current frame
alias emm="emacsclient -c -n"
## Open files quickly
alias emq="emacs -Q"
## Daemon
emdaemon() {
    emacsclient -e 't' &>/dev/null || emacs --daemon
}
## Dired
ef() {
    if [ -n "$1" ]; then
        emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \"$1\"))"
    else
        emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \"~\"))"
    fi
}

### Authentication agents
update_ssh_agent() {
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
    if [ ! -e $SSH_AUTH_SOCK ]; then
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
    fi
}

update_gpg_agent() {
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
    env HOMEBREW_NO_AUTO_UPDATE=1 PATH=/home/linuxbrew/.linuxbrew/bin:$PATH /home/linuxbrew/.linuxbrew/bin/brew @args
}

### Directory syncing
rsync_dir() {
    if [[ $# -lt 2 ]]; then
        echo "Usage: rsync_dir SRC DEST [EXCLUDE1,EXCLUDE2...]"
        return 1
    fi
    local src="$1"
    local dest="$2"
    local excludes="$([[ -n $3 ]] && echo --exclude={$3})"
    eval "rsync -avP --delete $excludes $src $dest" ## Prevent loss of curly braces
}
