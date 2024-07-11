### .bashrc  -- Bash init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Sanity checks
## Interactive mode only
[[ "$-" == *i* ]] || return 1

## Emacs TRAMP mode
[[ "$TERM" =~ [Dd]umb ]] && PS1="$ " && return 2

### Interactive shell initialization
## Global variables
declare -A RICE
RICE[root_dir]="$(dirname "$(realpath -s "${BASH_SOURCE[0]}")")" ## where this script is (no follow)
RICE[overlay]="${RICE[root_dir]}/.bashrc-overlay"
RICE[os_windows]=$([[ "$OS" =~ [Ww]indows ]] && echo 1)

## Commands
## Source a script file under the librice directory.
## Make this a closure so that the global `RICE' variable won't affect it
eval "function riceInc { local n=\"\${1:-}\" && shift; source \"${RICE[root_dir]}/.librice/\${n}\" \"\$@\"; }"

function string_join {
    ## Usage: srting-join delimiter string1 string2 ...
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}

### Set it up
## Environment settings
[[ "$1" == *noenv* ]] || {
    riceInc env/path
    riceInc env/gpg-agent
    riceInc env/gpg-agent-ssh
    riceInc env/prompt-classic
}

## History
shopt -s histappend
HISTCONTROL=ignoredups
HISTFILESIZE=100000
HISTSIZE=10000
PROMPT_COMMAND="history -a" # Update history immediately

## Random stuff
[[ -e "${RICE[overlay]}" ]] && source "${RICE[overlay]}"
