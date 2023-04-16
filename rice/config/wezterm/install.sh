#!/bin/bash

THIS_DIR=$(cd "$(dirname $BASH_SOURCE)" && pwd)
CONFIG_DIR="$HOME/.config/wezterm"

mkdir -p $CONFIG_DIR
cat <<EOF >>$CONFIG_DIR/wezterm.lua
local package = require "package"
package.path =  package.path .. ";$THIS_DIR/?.lua"
local conf = require "init"
conf = conf:rice_merge({
    -- Custom config goes here
    -- default_prog = {"pwsh", "-NoLogo"}
})
return conf
EOF
