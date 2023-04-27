### Fish setup

### Status check
if ! status is-interactive
    return
end

### Environment variables
set -g fish_greeting
set -g PATH $HOME/bin $PATH
## Use Emacs style
set -g fish_key_bindings fish_default_key_bindings
set -g fish_cursor_selection_mode exclusive
