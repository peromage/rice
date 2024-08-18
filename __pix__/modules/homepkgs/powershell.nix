{ config, lib, pix, pkgs, ... }:

let
  cfg = config.pix.homepkgs.powershell;
  src = "${pix.path.dotfiles}/pwsh/.config/powershell";

in with lib; {
  options.pix.homepkgs.powershell = {
    enable = mkEnableOption "PowerShell";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.powershell ];

    xdg.configFile."powershell" = {
      source = src;
      recursive = true;
    };
  };
}
