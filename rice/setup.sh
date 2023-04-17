#!/bin/bash

### Variables and functions
THIS_DIR=$(realpath -s $(dirname $BASH_SOURCE))

help() {
    cat <<EOF
$0 usage:
    -h, --help    Print this message
    --sync        Stash uncommitted changes, sync this repository and restore
    --stow        Use Stow to setup all configurations under "stow" directory
    --unstow      Delete all stowed packages
EOF
}

setup_sync() {
    cd $THIS_DIR
    [[ "No local changes to save" == $(git stash push) ]] && local pop_needed=1
    git pull origin HEAD
    [[ -n $pop_needed ]] && git stash pop
    if [[ -n "$(git diff --name-only --diff-filter=U)" ]]; then
        ## Conflicts
        echo -e "\e[31;1m>> FAILED: Conflicts found after sync\e[0m"
        exit 1
    fi
    echo ">>  SUCCEEDED: sync"
    exit 0
}

setup_stow() {
    stow -d "$THIS_DIR/stow" -t $HOME .
}

setup_unstow() {
    stow -d "$THIS_DIR/stow" -t $HOME -D .
}

### Script starts here
case "$1" in
    --sync)
        setup_sync
        ;;
    --stow)
        setup_stow
        ;;
    --unstow)
        setup_unstow
        ;;
    *)
        help
        exit 1
        ;;
esac
