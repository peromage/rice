{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.kde;
  cfgAll = config.rice.desktops;

in with lib; {
  options.rice.desktops.env.kde = mkDesktopOptions {
    name = "KDE";
  } // (with lib; {
    enableSDDM = mkEnableOption "SDDM display manager" // { default = true; };
  });

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
