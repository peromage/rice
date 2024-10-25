{ config, lib, pix, ... }:

let
  cfg = config.pix.homeprogs.zellij;
  src = "${pix.path.dotfiles}/zellij/.config/zellij";

in with lib; {
  options.pix.homeprogs.zellij = {
    enable = mkEnableOption "Zellij";
  };

  config = mkIf cfg.enable {
    programs.zellij.enable = true;

    xdg.configFile."zellij" = {
      source = src;
      recursive = true;
    };
  };
}
