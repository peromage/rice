{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.gnome;
  cfgAll = config.rice.desktops;

in with lib; {
  options.rice.desktops.env.gnome = {
    enable = mkEnableOption "Gnome desktop environment";
    disableGDM = mkEnableOption "GDM off";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = !cfg.disableGDM;
      displayManager.gdm.wayland = cfgAll.enableWayland;
    };
  };
}
