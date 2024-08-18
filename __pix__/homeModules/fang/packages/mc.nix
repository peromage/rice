{ pix, pkgs, ... }:

let
  src = "${pix.paths.dotfiles}/mc/.config/mc";

in {
  home.packages = [ pkgs.mc ];

  xdg.configFile."mc" = {
    source = src;
    recursive = true;
  };
}
