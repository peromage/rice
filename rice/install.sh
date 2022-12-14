#!/bin/bash

### Configuration
INSTALL_SCRIPT_NAME="install.sh"

### Function definitions
help() {
    cat <<EOF
$(generate_installable_module_list)
EOF
}

generate_installable_module_list() {
    for i in $(ls */$INSTALL_SCRIPT_NAME); do
        echo "${i%%/$INSTALL_SCRIPT_NAME}"
    done
}

validate_module_support() {
    ## Validate if input modules have install scripts defined
    local error=0
    local supported_modules=$(generate_installable_module_list)
    ## Stupid loop but works
    for i in $@; do
        for j in $supported_modules; do
            [[ $j =~ ^$i$ ]] && continue 2
        done
        echo "$i: No installation script found.  $i/$INSTALL_SCRIPT_NAME has to be provided."
        error=1
    done
    return $error
}

### Script starts here
## Change working directory
cd "$(dirname $BASH_SOURCE)"

## Print available modules when none specified
[[ $# -eq 0 ]] && help && exit 1

## Abort if one of the modules doesn't have install script defined
validate_module_support $@ || exit 1

## Install modules
for i in $@; do
    $i/$INSTALL_SCRIPT_NAME
done
