{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.kde;

  options = with lib; mkDesktopOptions {
    name = "KDE";
  } // {
    enableSDDM = mkEnableOption "SDDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.kde = options;

  config = with lib; mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
