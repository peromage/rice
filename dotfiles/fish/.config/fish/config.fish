### config.fish  -- Fish init -*- outline-regexp: "###\\(#* [^ \t\n]\\)"; -*-

### Pre-check
status is-interactive || exit

### Interactive initialization
set -g fish_greeting
## Use Emacs style
set -g fish_key_bindings fish_default_key_bindings
set -g fish_cursor_selection_mode exclusive

## Environment variables
fish_add_path $HOME/bin $HOME/.local/bin

### Commands
function easy_cd
    ## Use ".."
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
end
abbr --add dotdot --regex '^\.\.+$' --function easy_cd
