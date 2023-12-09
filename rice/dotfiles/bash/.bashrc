### .bashrc  -- Bash init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Commands
function rice_include {
    ## Source a .bash script file under librice directory.
    ## The name should be the file basename without extension .bash.
    ## Usage: rice_include name [args]
    local name="${1:-}"; shift
    source "${RICE[root_dir]}/.librice/${name}" "$@";
}

function string_join {
    ## Usage: srting-join delimiter string1 string2 ...
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}

### Pre-checks
## Environment settings
[[ "$1" == *noenv* ]] || {
    rice_include env/path
    rice_include env/gpg-agent
    rice_include env/gpg-agent-ssh
}

## Interactive mode only
[[ "$-" == *i* ]] || return 1

## Emacs TRAMP mode
[[ "$TERM" =~ [Dd]umb ]] && PS1="$ " && return 2

### Interactive shell initialization
## Global variables
declare -A RICE
RICE[root_dir]="$(dirname "$(realpath -s "${BASH_SOURCE[0]}")")" ## where this script is (no follow)
RICE[custom_rc]="${RICE[root_dir]}/.bashrc-custom"
RICE[os_windows]=$([[ "$OS" =~ [Ww]indows ]] && echo 1)

## History
shopt -s histappend
HISTCONTROL=ignoredups
HISTFILESIZE=100000
HISTSIZE=10000
PROMPT_COMMAND="history -a" # Update history immediately

## Prompt
rice_include env/prompt-classic

## Random stuff
[[ -e "${RICE[custom_rc]}" ]] && source "${RICE[custom_rc]}"
