{ pix, ... }:

let
  src = "${pix.path.dotfiles}/tmux/.config/tmux";

in {
  programs.tmux.enable = true;

  xdg.configFile."tmux" = {
    source = src;
    recursive = true;
  };
}
