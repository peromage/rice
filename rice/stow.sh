#!/usr/bin/env bash

OPTION="$1" && shift
SOURCE="$1" && shift

case "$OPTION" in
    -S|--stow|-D|--delete|-R|--restow) ;;
    *)
        echo "Invalid option: $OPTION"
        exit 1
        ;;
esac

cd "${BASH_SOURCE%/*}"

if ! test -d "$SOURCE"; then
    echo "Not a directory: $SOURCE"
    exit 1
fi

eval "stow --dir=$SOURCE --target=$HOME $OPTION ."
