#!/bin/bash
### init.bash -- Bootstrap for Bash

### Prerequisites

## Source guard
if [[ -z $BASH_VERSION ]] || [[ -n $LOADED_RICE_BASH ]]; then
    return
fi
LOADED_RICE_BASH=1

## Interactive mode only
case "$-" in
    *i*) ;;
    *) return;;
esac

## Directory where this script is
ribash_home=$(dirname $(realpath "$BASH_SOURCE"))

## Module source command
rinclude() {
    ## Source the given module file
    ## File name implies suffix ".sh"
    local file="$ribash_home/${1}.sh" && shift
    source $file $@
}

### Load module files

rinclude sh/init-env
rinclude sh/init-cmd
rinclude sh/init-cmd-win
rinclude sh/theme-normal
rinclude local
