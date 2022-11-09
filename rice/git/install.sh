#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)

cat <<EOF >>$HOME/.gitconfig
[include]
    path = $THIS_DIR/.gitconfig
EOF
