#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)
CONFIG_DIR="$HOME/.gnupg"
cd $THIS_DIR

mkdir -p $CONFIG_DIR
cat gpg-agent.conf >>$CONFIG_DIR/gpg-agent.conf
cat sshcontrol >>$CONFIG_DIR/sshcontrol
