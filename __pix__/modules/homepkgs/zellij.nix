{ config, lib, pix, ... }:

let
  cfg = config.pix.homepkgs.zellij;
  src = "${pix.path.dotfiles}/zellij/.config/zellij";

in with lib; {
  options.pix.homepkgs.zellij = {
    enable = mkEnableOption "Zellij";
  };

  config = mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };

    xdg.configFile."zellij" = {
      source = src;
      recursive = true;
    };
  };
}
