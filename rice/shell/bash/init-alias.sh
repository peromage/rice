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

### lf
lfcd() {
    local tmp=$(mktemp)
    lf -last-dir-path=$tmp $@
    [[ ! -f $tmp ]] && return
    local target=$(cat $tmp)
    rm -f $tmp
    [[ -d $target && $target != $(pwd) ]] && cd $target
}

### ranger
rf() {
    if [[ -n $RANGER_LEVEL ]]; then
        echo "Nested ranger!"
        return
    fi
    ranger $@
}

### fzf
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

### linuxbrew
brewenv() {
    alias brew=/home/linuxbrew/.linuxbrew/bin/brew
    eval "$(brew shellenv)"
    export PS1="(brew) $PS1"
}

brew() {
    env HOMEBREW_NO_AUTO_UPDATE=1 PATH=/home/linuxbrew/.linuxbrew/bin:$PATH /home/linuxbrew/.linuxbrew/bin/brew @args
}
