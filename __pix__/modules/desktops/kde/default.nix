{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfgAll = config.pix.desktops;
  cfg = config.pix.desktops.env.kde;

  options = mkDesktopOptions {
    name = "KDE";
  } // {
    enableSDDM = lib.mkEnableOption "SDDM display manager" // { default = true; };
  };

in {
  options.pix.desktops.env.kde = options;

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
