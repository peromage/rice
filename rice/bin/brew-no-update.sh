#!/usr/bin/env bash

env HOMEBREW_NO_AUTO_UPDATE=1 PATH="/home/linuxbrew/.linuxbrew/bin:${PATH}" /home/linuxbrew/.linuxbrew/bin/brew "$@"
