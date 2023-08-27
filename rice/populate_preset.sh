#!/usr/bin/bash

cd "${BASH_SOURCE%/*}"

if [ $# -lt 2 ]; then
    echo "Insufficient arguments"
    exit 1
fi

PRESET_DIR="$1" && shift
PACKAGE_DIR="config"

if [ -e "$PRESET_DIR" ] && [ ! -d "$PRESET_DIR" ]; then
    echo "Not a directory: $PRESET_DIR"
    exit 1
fi

test ! -d "$PRESET_DIR" && mkdir "$PRESET_DIR"
# test ! -d "$PRESET_DIR" && echo "creating $PRESET_DIR"

eval "stow --dir=config --target=$PRESET_DIR -S $@"
