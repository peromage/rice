#!/usr/bin/bash

### Variables and functions
THIS_DIR=$(realpath -s $(dirname $BASH_SOURCE))

function setup_help {
    cat <<EOF
$0 usage:
$(declare -F | perl -ne '/setup_(.+)/ and print "  --$1\n"')
EOF
}

function setup_sync_with_stash {
    cd $THIS_DIR
    [[ "No local changes to save" == $(git stash push) ]] && local pop_needed=1
    git pull
    [[ -n $pop_needed ]] && git stash pop
    if [[ -n "$(git diff --name-only --diff-filter=U)" ]]; then
        ## Conflicts
        echo -e "\e[31;1m>> FAILED: Conflicts found after sync\e[0m"
        exit 1
    fi
    echo ">>  SUCCEEDED: sync_with_stash"
    exit 0
}

function setup_sync {
    cd $THIS_DIR
    git pull
    echo ">>  SUCCEEDED: sync"
    exit 0
}

function setup_stow {
    ## Make sure no accidental db submission
    mkdir $HOME/.gnupg 2>/dev/null && chmod 700 $HOME/.gnupg 2>/dev/null
    mkdir $HOME/bin 2>/dev/null
    stow -d "$THIS_DIR/stow" -t $HOME .
}

function setup_unstow {
    stow -d "$THIS_DIR/stow" -t $HOME -D .
}

### Script starts here
opt=${1:-} && shift
if ! setup_help | grep -Pq -- "^ *$opt *$"; then
    setup_help
    printf "\nNot a valid option: $opt\n"
    exit 1
fi

setup_${opt#--} $@
