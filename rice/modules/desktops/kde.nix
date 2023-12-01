{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.kde;
  cfgAll = config.rice.desktops;

in {
  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = !cfg.disableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
