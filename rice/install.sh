#!/bin/bash

cd "$(dirname $BASH_SOURCE)"
INSTALL_SCRIPT="install.sh"

scan_modules() {
    for i in $(ls */$INSTALL_SCRIPT); do
        echo "${i%%/$INSTALL_SCRIPT}"
    done
}

main() {
    ## Print available modules when none specified
    if [[ $# -eq 0 ]]; then
        cat <<EOF
Usage: $(basename $0) MODULE1 MODULE2 ...
Available modules to be installed:
$(scan_modules)
EOF
        exit 1
    fi
    ## Validate input modules before running installation scripts
    local stop=0
    local modules=$(scan_modules)
    ## Stupid but works
    for i in $@; do
        for j in $modules; do
            [[ $j =~ ^$i$ ]] && continue 2
        done
        echo "$i: No installation script found. $i/$INSTALL_SCRIPT has to be provided."
        stop=1
    done
    [[ $stop -ne 0 ]] && exit 1

    for i in $@; do
        $i/$INSTALL_SCRIPT
    done
}

## Start script
main $@
