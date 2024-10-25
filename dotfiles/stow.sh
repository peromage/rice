#!/usr/bin/env bash

set -e

function usage {
    >&2 cat <<EOF
Usage:
  $0 ACTION [OPTIONS] pkg1 [pkg2 [...]]

Actions:
  -S    Stow packages
  -D    Delete packages
  -R    Restow packages

Options:
  -f    Enable folding (Default: no folding)
  -h    Show this message
  -v    Verbose output
EOF
}

## Need to append action and package name
BASECMD="stow --dir=${BASH_SOURCE[0]%/*} --target=$HOME"

action=
opts="folding=0 verbose=0"
package_list=()

function run_stow {
    local action="$1" && shift
    local opts="$1" && shift
    local args=
    ! [[ "$opts" == *folding=1* ]] && args="--no-folding $args"
    [[ "$opts" == *verbose=1* ]] && args="-v $args"

    for p in "$@"; do
        local cmd="${BASECMD} ${args} ${action} ${p}"
        echo "> RUN: $cmd"
        eval "$cmd"
    done
}

while [[ "$OPTIND" -le $# ]]; do
    if getopts "SDRfhv" o; then
        case "$o" in
            (S) action="-S" ;;
            (D) action="-D" ;;
            (R) action="-R" ;;
            (f) opts="folding=1 $opts" ;;
            (v) opts="verbose=1 $opts" ;;
            (h) ;&
            (*) usage; exit 1 ;;
        esac
    else
        eval "package_list+=("${!OPTIND}")" # Indirect reference
        let OPTIND++
    fi
done


if [[ -z $action ]] || [[ ${#package_list} -eq 0 ]]; then
    usage
    exit 1
fi

run_stow $action "$opts" "${package_list[@]}"
