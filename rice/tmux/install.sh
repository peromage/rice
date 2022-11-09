#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)

cat <<EOF >>$HOME/.tmux.conf
source $THIS_DIR/.tmux.conf
EOF
