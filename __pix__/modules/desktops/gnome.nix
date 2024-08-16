{ config, lib, pkgs, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.profiles.gnome;

in {
  options.pix.desktops.profiles.gnome = with lib; {
    enable = mkEnableOption "Gnome";
    enableGDM = mkEnableOption "GDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = cfg.enableGDM;
      displayManager.gdm.wayland = cfgOverall.enableWayland;
    };

    environment.systemPackages = with pkgs.gnomeExtensions; [
      tray-icons-reloaded
    ];
  };
}
