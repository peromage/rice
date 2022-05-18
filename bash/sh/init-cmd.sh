### init-cmd.sh -- Commands

## Common aliases
alias ll="ls -lahF --color=auto"

## Emacs: Open files in the terminal
alias em="emacsclient -c -nw"
## Emacs: Open files in the current frame
alias emm="emacsclient -c -n"
## Emacs: Daemon
alias emdaemon="emacs --daemon"
## Emacs: Dired
ef() {
    if [ -n "$1" ]; then
        emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \"$1\"))"
    else
        emacs -Q -nw --eval "(progn (xterm-mouse-mode 1) (dired \"~\"))"
    fi
}

## lf
lfcd() {
    local tmp=$(mktemp)
    lf -last-dir-path=$tmp $@
    [[ ! -f $tmp ]] && return
    local target=$(cat $tmp)
    rm -f $tmp
    [[ -d $target && $target != $(pwd) ]] && cd $target
}

## ranger
rf() {
    if [[ -n $RANGER_LEVEL ]]; then
        echo "Nested ranger!"
        return
    fi
    ranger $@
}

## fzf
ffdo() {
    if [[ -z $1 ]]; then
        echo "Usage: ffdo <cmd> [arguments]"
        return
    fi
    local cmd=$1 && shift
    local target="$(fzf)"
    [[ -n $target ]] && eval "$cmd $@ $target"
}

ffcd() {
    local target="$(fzf)"
    [[ -n $target ]] && cd $target
}

## linuxbrew
brewenv() {
    alias brew=/home/linuxbrew/.linuxbrew/bin/brew
    eval "$(brew shellenv)"
    export PS1="(brew) $PS1"
}

brew() {
    env HOMEBREW_NO_AUTO_UPDATE=1 PATH=/home/linuxbrew/.linuxbrew/bin:$PATH /home/linuxbrew/.linuxbrew/bin/brew @args
}

## Authentication agents
update-ssh-agent() {
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
    if [[ ! -e $SSH_AUTH_SOCK ]]; then
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
    fi
}

update-gpg-agent() {
    unset SSH_AGENT_PID
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye > /dev/null
}
