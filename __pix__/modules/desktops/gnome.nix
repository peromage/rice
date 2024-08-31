{ config, lib, pkgs, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.gnome;

in with lib; {
  options.pix.desktops.env.gnome = {
    enable = mkEnableOption "Gnome";
    enableGDM = mkEnableOption "GDM display manager" // { default = true; };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = cfg.enableGDM;
      displayManager.gdm.wayland = cfgOverall.enableWayland;
    };

    environment.systemPackages =
      (with pkgs; [
        gnome-terminal ## Provides more functionalities than default gnome-console
      ])
      ++ (with pkgs.gnomeExtensions; [
      tray-icons-reloaded
    ]);
  };
}
