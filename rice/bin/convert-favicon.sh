#!/usr/bin/env bash

## Requires `imagemagick'

if [ $# -lt 2 ]; then
    echo "$0 <INPUT> <OUTPUT> [-remove-bg]"
    exit 1
fi

input="$1" && shift
output="$1" && shift
if [ "-remove-bg" == "$1" ]; then
    input_args="-background transparent"
fi
output_args="-define icon:auto-resize=16,24,32,48,64,72,96,128,256"
convert $input_args "$input" $output_args "$output"
