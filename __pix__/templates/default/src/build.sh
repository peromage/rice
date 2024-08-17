#!/usr/bin/env bash

set -e

function usage {
    >&2 cat <<EOF
Usage:
    $0 <build | boot> [name]
EOF
}

cwd=$(realpath -s "${BASH_SOURCE[0]%/*}")

## Parameters
action="$1"
shift || { usage; exit 1; }

name="$1"
shift || name="default"

## Run flake
nix flake update
case "$action" in
    build) nixos-rebuild build --flake "${cwd}#${name}" --show-trace ;;
    boot) sudo nixos-rebuild boot --flake "${cwd}#${name}" --show-trace ;;
    *) usage; exit 1 ;;
esac
