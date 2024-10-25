{ config, lib, pix, pkgs, ... }:

let
  cfg = config.pix.homeprogs.mc;
  src = "${pix.path.dotfiles}/mc/.config/mc";

in with lib; {
  options.pix.homeprogs.mc = {
    enable = mkEnableOption "Midnight Commander";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.mc ];

    xdg.configFile."mc" = {
      source = src;
      recursive = true;
    };
  };
}
