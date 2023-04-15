#!/bin/bash

### Configuration
INSTALL_SCRIPT_NAME="install.sh"
THIS_DIR=$(realpath -s $(dirname $BASH_SOURCE))

### Function definitions
help() {
    cat <<EOF
 --- Available moduels ---
$(list_modules)
EOF
}

## Any subdirectory that has $INSTALL_SCRIPT_NAME at the root
list_modules() {
    for i in $(ls */$INSTALL_SCRIPT_NAME); do
        echo "${i%%/$INSTALL_SCRIPT_NAME}"
    done
}

## This function should be executed at the root of the rice directory
install() {
    local failed=""
    local succeeded=""
    for i in $@; do
        if $i/$INSTALL_SCRIPT_NAME; then
            succeeded="$succeeded $i"
        else
            failed="$failed $i"
        fi
    done
    for i in $succeeded; do
        echo ">> SUCCEEDED: $i"
    done
    [[ -z "$failed" ]] && exit 0
    ## Print failed ones
    for i in $failed; do
        echo ">> FAILED: $i"
    done
    exit 1
}

### Script starts here
cd $THIS_DIR

[[ 0 -eq $# ]] && list_modules && exit 1
[[ "-h" == "$1" ]] || [[ "--help" == "$1" ]] && help && exit 1

install $@
