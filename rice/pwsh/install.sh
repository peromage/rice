#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)
CONFIG_DIR="$HOME/.config/powershell"

mkdir -p $CONFIG_DIR
cat <<EOF >>$CONFIG_DIR/profile.ps1
. $THIS_DIR/init.ps1
EOF
