{ config, pkgs, lib, ... }:

let
  cfg = config.pix.homeprogs.nodejs;
  n = pkgs.pixPkgs.nodejs;
  nodejs = n.override {
    userNpmDir = "${config.xdg.dataHome}/${n.userNpmDir}";
  };

in with lib; {
  options.pix.homeprogs.nodejs = {
    enable = mkEnableOption "NodeJS";
  };

  config = mkIf cfg.enable {
    home.sessionPath = [
      nodejs.userPath
    ];
    home.packages = [ nodejs ];
    home.file.".npmrc".text = ''
      prefix=/home/fang/.local/share/npm-packages
    '';
  };
}
