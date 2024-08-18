{ pix, ... }:

let
  src = "${pix.path.dotfiles}/alacritty/.config/alacritty";

in {
  programs.alacritty.enable = true;

  xdg.configFile."alacritty" = {
    source = src;
    recursive = true;
  };
}
