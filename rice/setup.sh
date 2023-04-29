#!/usr/bin/bash
set -e

### Variables and functions
THIS_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
cd "$THIS_DIR"

function setup_help {
    cat <<EOF
$0 usage:
$(declare -F | perl -ne '/setup_(.+)/ and print "  --$1\n"')

Available presets:
$(find ./* -type d -prune | perl -ne '/PRESET-(.+)/ and print "  $1\n"')
EOF
}

function setup_sync_with_stash {
    [[ "No local changes to save" == $(git stash push) ]] || local pop_needed=1
    git pull
    [[ -n $pop_needed ]] && git stash pop
    if [[ -n "$(git diff --name-only --diff-filter=U)" ]]; then
        ## Conflicts
        echo -e "\e[31;1mConflicts found after sync\e[0m"
        exit 1
    fi
}

function setup_sync {
    git pull
}

function prepare_dir {
    ## Make sure no accidental db submission
    mkdir "$HOME/.gnupg" 2>/dev/null && chmod 700 "$HOME/.gnupg" 2>/dev/null
    mkdir "$HOME/bin" 2>/dev/null
}

function setup_stow {
    prepare_dir
    stow -d config -t "$HOME" "$@"
}

function setup_unstow {
    stow -d config -t "$HOME" -D "$@"
}

function setup_preset {
    prepare_dir
    ## Let errors be thrown by stow
    stow -d "PRESET-$1" -t "$HOME" .
}

function setup_unpreset {
    stow -d "PRESET-$1" -t "$HOME" -D .
}

### Script starts here
opt="${1:-}"
shift || true ## Ignore 0 argument shifting
if [[ -z "$opt" ]] || ! setup_help | grep -Pq -- "^ *$opt *$"; then
    setup_help
    printf "\n%s\n" "Not a valid option: $opt"
    exit 1
fi

"setup_${opt#--}" "$@"
