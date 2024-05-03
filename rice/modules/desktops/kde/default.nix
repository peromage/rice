{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.kde;

  options = mkDesktopOptions {
    name = "KDE";
  } // {
    enableSDDM = mkEnableOption "SDDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.kde = options;

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
