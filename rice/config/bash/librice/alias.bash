### alias.bash -- Bash aliases

### linuxbrew
brewenv() {
    eval "$(brew shellenv)"
    export PS1="(brew) $PS1"
}

### Usage: srting-join delimiter string1 string2 ...
string-join() {
    local d="${1:-}" f="${2:-}"
    if shift 2; then
        printf "%s" "$f" "${@/#/$d}"
    fi
}
