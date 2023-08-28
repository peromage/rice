### .bashrc  -- Bash init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Pre-checks #################################################################
## Source guard
[[ -z $BASH_VERSION ]] && return 1
## Interactive mode only
[[ ! "$-" =~ "i" ]] && return 2
## Emacs TRAMP mode
[[ "$TERM" =~ [Dd]umb ]] && PS1="$ " && return 3

### Environment variables ######################################################
declare -A RICE
RICE[rc]="$(dirname "$(realpath -s "${BASH_SOURCE[0]}")")" ## where this script is (no follow)
RICE[custom_rc]="${RICE[rc]}/custom.bash"
RICE[os_windows]=$([[ "$OS" =~ [Ww]indows ]] && echo 1)

### Commands ###################################################################
function rice_include {
    ## Source a .bash script file under librice directory.
    ## The name should be the file basename without extension .bash.
    ## Usage: rice_include name [args]
    local name="${1:-}"; shift
    source "${RICE[rc]}/librice/${name}" "$@";
}

function string_join {
    ## Usage: srting-join delimiter string1 string2 ...
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}

### Environment settings #######################################################
rice_include env.sh prompt-classic path gpg-agent

### Random stuff ###############################################################
[[ -e "${RICE[custom_rc]}" ]] && source "${RICE[custom_rc]}"
