#!/usr/bin/bash

cd "$(dirname $BASH_SOURCE)/.."

TARGET_BRANCH=master

if [[ $(git branch --show-current) != $TARGET_BRANCH ]]; then
    echo "Checkout master branch to proceed!"
    exit 1
fi

git subtree push --prefix rice git@github.com:peromage/rice.git $TARGET_BRANCH
