#!/usr/bin/env bash

ACTION="$1" && shift
SOURCE="$1" && shift

case "$ACTION" in
    -S|--stow|-D|--delete|-R|--restow) ;;
    *)
        >&2 cat <<EOF
Invalid action: $ACTION

Usage:
  $0 action path-to-package-top [arguments]

Actions:
  -S, --stow
  -D, --delete
  -R, --restow
EOF
        exit 1
        ;;
esac

cd "${BASH_SOURCE%/*}"

if ! test -d "$SOURCE"; then
    echo "Not a directory: $SOURCE"
    exit 1
fi

CMD="stow $@ --dir=$SOURCE --target=$HOME $ACTION ."
echo "Invoking: $CMD"
eval "$CMD"
