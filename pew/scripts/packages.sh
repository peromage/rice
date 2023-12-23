#!/usr/bin/env bash

set -e

TOP="$(cd ${BASH_SOURCE[0]%/*} && git rev-parse --show-toplevel)"
FILENAME="elpa"

[[ -d "${TOP}/pew" ]] || { echo "Git top level is not Pew"; exit 1; }

echo "Packing compiled packages"
tar -C "$TOP" -czf "${FILENAME}.tar.gz" elpa elpa-*
