#!/bin/bash
### .bashrc -- Bootstrap for Bash

### Prerequisites
## Source guard
[[ -z $BASH_VERSION ]] && return 1
## Interactive mode only
[[ ! "$-" =~ "i" ]] && return 2
## Emacs TRAMP mode
[[ "$TERM" =~ "[Dd]umb" ]] && PS1="$ " && return 3

## Global variables
declare -A RICE
RICE[rc]=$(realpath -s $(dirname $BASH_SOURCE)) ## where this script is (no follow)
RICE[custom_rc]="${RICE[rc]}/custom.bash"
RICE[os_windows]=$([[ "$OS" =~ "[Ww]indows" ]] && echo 1)

## Equivalent to: source librice/module [args]
function rice_include { local f="${1:-}" && shift && source "${RICE[rc]}/librice/${f}" $@; }

### Loading module files
rice_include alias.bash
rice_include prompts/classic.sh

### Environment
rice_include env.sh path gpg-agent

### Random stuff starts here
[[ -e "${RICE[custom_rc]}" ]] && source "${RICE[custom_rc]}"
