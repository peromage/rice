#!/bin/bash

## Config commands
## Each command function name has to be "my_NAME"
my_bash() {
    cat <<EOF | append $HOME/.bashrc
source $RICE_HOME/bash/init.sh
update-gpg-agent
EOF
}

my_pwsh() {
    cat <<EOF | append $HOME/.config/powershell/profile.ps1
. $RICE_HOME/pwsh/init.ps1
EOF
}

my_git() {
    cat <<EOF | append $HOME/.gitconfig
[include]
    path = $RICE_HOME/git/.gitconfig
EOF
}

my_tmux() {
    cat <<EOF | append $HOME/.tmux.conf
source $RICE_HOME/tmux/.tmux.conf
EOF
}

my_gnupg() {
    cat $RICE_HOME/gnupg/gpg-agent.conf | append $HOME/.gnupg/gpg-agent.conf
    cat $RICE_HOME/gnupg/sshcontrol | append $HOME/.gnupg/sshcontrol
}

my_vim() {
    cat <<EOF | append $HOME/.vimrc
source $RICE_HOME/vim/init.vim
EOF
}

my_neovim() {
    cat <<EOF | append $HOME/.config/nvim/init.vim
source $RICE_HOME/vim/init.vim
EOF
}

my_alacritty() {
    cat <<EOF | append $HOME/.alacritty.yml
import:
  - $RICE_HOME/alacritty/alacritty.yml
EOF
}

## Utilities
RICE_HOME=$(dirname $(realpath $BASH_SOURCE))

makedir() {
    mkdir -p $(dirname $1)
}

append() {
    ## $1: File path
    makedir $1
    cat >> $1
    echo "Appended to: $1"
}

overwrite() {
    ## $1: File path
    makedir $1
    cat > $1
    echo "Overwrote: $1"
}

## CLI
SUBCMD_PREFIX="my_"

exec_subcmd() {
    local subcmd="$SUBCMD_PREFIX$1" && shift
    if [[ "function" == $(type -t $subcmd) ]]; then
        eval "$subcmd $@"
    else
        echo "Not found: $subcmd"
    fi
}

print_subcmd() {
    echo "Available CMD:"
    declare -F | grep -Po "(?<=declare -f $SUBCMD_PREFIX)[a-zA-Z0-9._-]+"
    exit 1
}

run() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: $(basename $0) CMD ARGS"
        print_subcmd
    fi
    exec_subcmd $@
}

runn() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: $(basename $0) CMD CMD ..."
        print_subcmd
    fi
    ## Iterate through subcommands
    for i in $@; do
        exec_subcmd $i
    done
}

## Start script
runn $@
