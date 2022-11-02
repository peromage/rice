#!/bin/bash
### init.sh -- Bootstrap for Bash

### Prerequisites
## Source guard
[[ -z $BASH_VERSION ]] && return 1
## Interactive mode only
[[ ! "$-" =~ "i" ]] && return 2
## Emacs TRAMP mode
[[ "$TERM" =~ "[Dd]umb" ]] && PS1="$ " && return 3

### Initialization
## Rice variables
declare -A rice=(
    [home]=$(dirname $(realpath "$BASH_SOURCE"))
    [platform_windows]=$([[ "$OS" =~ "[Ww]indows" ]] && echo 1)
)

## Source a module whose path is relative to this file
rice_include() {
    local file="$1"
    source "${rice[home]}/${file}" $@
}

### Environment
export PATH="$PATH:$(dirname ${rice[home]})/scripts"
export EDITOR="vim"

### Load module files
rice_include modules/init-alias.sh
rice_include modules/theme-classic.sh

if [[ -n "${rice[platform_windows]}" ]]; then
    rice_include modules/init-alias-win.sh
fi
