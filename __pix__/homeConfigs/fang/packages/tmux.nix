{ pix, ... }:

let
  src = "${pix.paths.dotfiles}/tmux/.config/tmux";

in {
  programs.tmux.enable = true;

  xdg.configFile."tmux" = {
    source = src;
    recursive = true;
  };
}
