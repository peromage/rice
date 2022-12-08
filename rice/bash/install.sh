#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)

cat <<EOF >>$HOME/.bashrc
source $THIS_DIR/init.sh
reload-gpg-agent
EOF

cat <<EOF >>$HOME/.bash_profile
## Add section names as arguments to enable their environment settings
source $THIS_DIR/env.sh
EOF
