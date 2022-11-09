#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)

cat <<EOF >>$HOME/.bashrc
source $THIS_DIR/init.sh
update-gpg-agent
EOF
