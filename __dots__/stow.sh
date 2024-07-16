#!/usr/bin/env bash

## The rest are package names
# ACTION="$1" && shift
# SOURCE="$1" && shift

set -e

function usage {
    >&2 cat <<EOF
Usage:
  $0 ACTION [pkg1 [pkg2 [...]]] ACTION [pkg3 [pkg4 [...]]] ...

Actions:
  -s    Stow packages
  -d    Delete packages
  -r    Restow packages

Multiple actions can be specified at the same time.  For example:
  $0 -s bash fish -d pwsh -r tmux

EOF
}

stow_list=()
delete_list=()
restow_list=()
current_list=
verbose_mode=

## Need to append action and package name
base_cmd="stow --no-folding --dir=${BASH_SOURCE[0]%/*} --target=$HOME"

function run_stow {
    local act="$1" && shift
    for p in "$@"; do
        local cmd="${base_cmd} ${act} ${p}"
        [[ -n "$verbose_mode" ]] && echo "> CMD: $cmd"
        eval "$cmd"
    done
}

while [[ "$OPTIND" -le $# ]]; do
    if getopts "sdrv" o; then
        case "$o" in
            s) current_list=stow_list ;;
            d) current_list=delete_list ;;
            r) current_list=restow_list ;;
            v) verbose_mode=1 ;;
            *) usage; exit 1 ;;
        esac
    else
        if [[ -n "$current_list" ]]; then
            eval "${current_list}+=("${!OPTIND}")" # Indirect reference
            let OPTIND++
        else
            usage
            exit 1
        fi
    fi
done

action_performed=0

if [[ ${#stow_list} -eq 0 ]] && [[ ${#delete_list} -eq 0 ]] && [[ ${#restow_list} -eq 0 ]]; then
    usage
    exit 1
fi

run_stow -S "${stow_list[@]}"
run_stow -D "${delete_list[@]}"
run_stow -R "${restow_list[@]}"
