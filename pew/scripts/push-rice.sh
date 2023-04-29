#!/usr/bin/bash

TARGET_BRANCH=master

if [[ ! -d rice ]]; then
    echo "Not at the repo root directory!"
    exit 1
fi

if [[ $(git branch --show-current) != "$TARGET_BRANCH" ]]; then
    echo "Checkout branch \"$TARGET_BRANCH\" to proceed!"
    exit 1
fi

git subtree push --prefix rice git@github.com:peromage/rice.git $TARGET_BRANCH
