
{ rice, ... }:

let
  src = "${rice.paths.dotfiles}/alacritty/.config/alacritty";

in {
  programs.alacritty.enable = true;

  xdg.configFile."alacritty" = {
    source = src;
    recursive = true;
  };
}
