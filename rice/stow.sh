#!/usr/bin/bash

cd "${BASH_SOURCE%/*}"

OPTION="$1" && shift
TARGET="$1" && shift

case "$OPTION" in
    -S|--stow|-D|--delete|-R|--restow) ;;
    *)
        echo "Invalid option: $OPTION"
        exit 1
        ;;
esac

if ! test -d "$TARGET"; then
    echo "Not a directory: $TARGET"
    exit 1
fi

eval "stow --dir=$TARGET --target=$HOME $OPTION ."
