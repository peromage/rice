### config.fish  -- Fish init -*- outline-regexp: "###\\(#* \\)"; -*-

### Status check
if ! status is-interactive
    exit
end

### Environment variables
set -g fish_greeting
set -g PATH $HOME/bin $PATH
## Use Emacs style
set -g fish_key_bindings fish_default_key_bindings
set -g fish_cursor_selection_mode exclusive
## Add to $fish_user_paths
fish_add_path $HOME/bin $HOME/.local/bin

### Commands
function easy_cd
    ## Use ".."
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function easy_cd

function pack_stuff
    argparse -N 1 "d/delete" -- $argv
    or return

    if set -q _flag_d
        set remove_files --remove-files
    end
    for i in $argv
        tar $remove_files -czvf (basename $i).tar.gz $i
    end
end
