#!/bin/bash

if [[ $# -lt 2 ]]; then
    echo "Usage: $(basename $0) SRC DEST [EXCLUDE1,EXCLUDE2...]"
    exit 1
fi

src="$1"
dest="$2"
excludes="$([[ -n $3 ]] && echo --exclude={$3})"

eval "rsync -avP --delete $excludes $src $dest" ## Prevent loss of curly braces
