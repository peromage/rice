#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)
VIM_DIR=$(cd "$THIS_DIR/../vim" && pwd)
CONFIG_DIR="$HOME/.config/nvim"

mkdir -p $CONFIG_DIR
cat <<EOF >>$CONFIG_DIR/init.vim
source $VIM_DIR/init.vim
EOF
