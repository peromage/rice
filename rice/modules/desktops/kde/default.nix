{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.kde;

  options = mkDesktopOptions {
    name = "KDE";
  } // {
    enableSDDM = lib.mkEnableOption "SDDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.kde = options;

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
