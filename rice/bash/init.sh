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
source "${BASH_SOURCE%/*}/modules/init-base.sh"

### Environment
export PATH=$(rice_join ":" "$PATH" \
    "$(realpath -s ${rice[home]}/../scripts)" \
    "${HOME}/.dotnet/tools" \
    "${HOME}/bin")
export EDITOR="vim"

### Load module files
rice_include modules/init-alias.sh
rice_include modules/theme-classic.sh

if [[ -n "${rice[platform_windows]}" ]]; then
    rice_include modules/init-alias-win.sh
fi
