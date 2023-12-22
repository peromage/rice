#!/usr/bin/env bash

set -e

TOP="$(cd ${BASH_SOURCE[0]%/*} && git rev-parse --show-toplevel)"
TEMP="$(mktemp -d)"
CONFIG_DIR=".emacs.d"
TARGET="${TEMP}/${CONFIG_DIR}"
BRANCH="master"
FILENAME="pew-cached"

[[ -d "${TOP}/pew" ]] || { echo "Git top level is not Pew"; exit 1; }

git clone --branch "$BRANCH" "$TOP" "$TARGET"
rsync -av "${TOP}/elpa" "${TOP}/elpa-"* "${TARGET}/"

echo "Packing $TARGET"
tar -C "$TEMP" -czf "${FILENAME}.tar.gz" "$CONFIG_DIR"

echo "Cleaning $TEMP"
rm -rf "$TEMP"
