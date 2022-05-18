#!/bin/bash

if [[ ! -f ./install.sh ]]; then
    echo "Working directory needs to be the root of rice"
    exit 1
fi

### Config commands
## Each command function name has to be "NAME_conf"

bash_conf() {
    cat <<EOF | append $HOME/.bashrc
source $RICE_HOME/bash/init.sh
EOF
    cat <<EOF | append $RICE_HOME/bash/local.sh
update-gpg-agent
EOF
}

git_conf() {
    cat <<EOF | append $HOME/.gitconfig
[include]
    path = $RICE_HOME/git/.gitconfig
EOF
}

tmux_conf() {
    cat <<EOF | append $HOME/.tmux.conf
source $RICE_HOME/tmux/.tmux.conf
EOF
}

gnupg_conf() {
    cat $RICE_HOME/gnupg/gpg-agent.conf | append $HOME/.gnupg/gpg-agent.conf
    cat $RICE_HOME/gnupg/sshcontrol | append $HOME/.gnupg/sshcontrol
}

### Utilities

RICE_HOME=$(pwd)

append() {
    ## $1: File path
    cat >> $1
    echo "Appended to: $1"
}

overwrite() {
    ## $1: File path
    cat > $1
    echo "Overwrote: $1"
}

config() {
    ## $1: Config name
    ## NOTE: Name implies "_conf" suffix
    if [[ "function" == $(type -t "$1_conf") ]]; then
        eval "$1_conf"
    else
        echo "Unknown: $1"
    fi
}

### Main entry

## List all available config if no components supplied
if [[ $# -eq 0 ]]; then
    echo "Usage: $(basename $0) CONFIG CONFIG ..."
    echo "Available configs:"
    declare -F | grep -Po '(?<=declare -f )[a-zA-Z0-9_-]+(?=_conf$)'
    exit 1
fi

## Iterate components
for i in $@; do
    config $i
done
