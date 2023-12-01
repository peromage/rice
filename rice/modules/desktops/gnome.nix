{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.gnome;
  cfgAll = config.rice.desktops;

in {
  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = !cfg.disableGDM;
      displayManager.gdm.wayland = cfgAll.enableWayland;
    };
  };
}
