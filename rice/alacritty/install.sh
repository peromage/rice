#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)

cat <<EOF >>$HOME/.alacritty.yml
import:
  - $THIS_DIR/alacritty.yaml
EOF
