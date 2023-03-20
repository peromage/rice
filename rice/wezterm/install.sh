#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)
CONFIG_DIR="$HOME/.config/wezterm"

mkdir -p $CONFIG_DIR
cat <<EOF >>$CONFIG_DIR/wezterm.vim
local conf = dofile("$THIS_DIR/init.lua")
conf = conf:rice_merge({
    -- Custom config goes here
    -- default_prog = {"pwsh", "-NoLogo"}
})
return conf
EOF
