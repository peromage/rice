### base.bash -- The fist script to be sourced

### Rice variables
declare -A rice=(
    [home]=$(realpath -s "${BASH_SOURCE%/*}/..")
    [platform_windows]=$([[ "$OS" =~ "[Ww]indows" ]] && echo 1)
)

### Source a module whose path is relative to this file
function rice_include {
    local file="$1"
    source "${rice[home]}/${file}" $@
}

### String concatenation
function rice_join {
    local d="${1-}" f="${2-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}
