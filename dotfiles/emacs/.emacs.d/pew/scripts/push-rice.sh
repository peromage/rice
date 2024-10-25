#!/usr/bin/env bash

TOP="$(cd ${BASH_SOURCE[0]%/*} && git rev-parse --show-toplevel)"
BRANCH="master"
PREFIX="rice"
URL="git@github.com:peromage/rice.git"

[[ -d "${TOP}/pew" ]] || { echo "Git top level is not Pew"; exit 1; }

[[ $(git branch --show-current) == "$BRANCH" ]] || { echo "Not on $BRANCH branch"; exit 1; }

git subtree push --prefix "$PREFIX" "$URL" "$BRANCH"
