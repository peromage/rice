{ mkDesktopOptions, ... }:
{ config, lib, pkgs, ... }:

let
  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.gnome;

  options = with lib; mkDesktopOptions {
    name = "Gnome";
  } // {
    enableGDM = mkEnableOption "GDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.gnome = options;

  config = with lib; mkIf cfg.enable {
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
