#!/usr/bin/bash

## A simple wrapper of stow

cd $(realpath -s $(dirname $BASH_SOURCE))
stow -t $HOME $@
