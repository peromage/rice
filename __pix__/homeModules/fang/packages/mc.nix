{ pix, pkgs, ... }:

let
  src = "${pix.path.dotfiles}/mc/.config/mc";

in {
  home.packages = [ pkgs.mc ];

  xdg.configFile."mc" = {
    source = src;
    recursive = true;
  };
}
