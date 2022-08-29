#!/bin/bash
### init.bash -- Bootstrap for Bash

### Prerequisites
## Source guard
if [[ -z $BASH_VERSION ]] || [[ -n $LOADED_RICE_BASH ]]; then
    return 1
fi
LOADED_RICE_BASH=1

## Interactive mode only
case "$-" in
    *i*) ;;
    *) return 2 ;;
esac

## Emacs TRAMP mode
if [[ "$TERM" == "dumb" ]]; then
    PS1="$ "
    return 3
fi

### Initialization
## Directory where this script is
ribash_home=$(dirname $(realpath "$BASH_SOURCE"))

## Module source command
rinclude() {
    ## $1: The module file that is to be sourced. File name implies suffix ".sh"
    ## $2: If non-empty, ignore errors if the module file doesn't exist
    local file="$ribash_home/${1}.sh"
    if [[ -n $2 ]] && [[ ! -f $file ]]; then
        return 100
    fi
    source $file
}

### Load module files
rinclude bash/init-default
rinclude bash/init-alias
rinclude bash/init-alias-win
rinclude bash/theme-canonical
