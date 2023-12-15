{ mkDesktopOptions, ... }:
{ config, lib, pkgs, ... }:

let
  cfg = config.rice.desktops.env.gnome;
  cfgAll = config.rice.desktops;

in with lib; {
  options.rice.desktops.env.gnome = mkDesktopOptions {
    name = "Gnome";
  } // (with lib; {
    enableGDM = mkEnableOption "GDM display manager" // { default = true; };
  });

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = cfg.enableGDM;
      displayManager.gdm.wayland = cfgAll.enableWayland;
    };

    environment.systemPackages = with pkgs.gnomeExtensions; [
      tray-icons-reloaded
    ];
  };
}
