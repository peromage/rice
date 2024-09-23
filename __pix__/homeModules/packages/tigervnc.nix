{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.homepkgs.tigervnc;
  src = "${pix.path.dotfiles}/systemd/.config/systemd/user/x0vncserver.service";

in with lib; {
  options.pix.homepkgs.tigervnc = {
    enable = mkEnableOption "TigerVNC";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ tigervnc ];
    xdg.configFile.x0vncserverService = {
      source = src;
      target = "systemd/user/x0vncserver.service";
    };
  };
}
