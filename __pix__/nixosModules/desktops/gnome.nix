{ config, lib, pkgs, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.gnome;

in with lib; {
  options.pix.desktops.env.gnome = {
    enable = mkEnableOption "Gnome";
    enableGDM = mkEnableOption "GDM display manager" // { default = true; };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.xserver = {
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = cfg.enableGDM;
        displayManager.gdm.wayland = cfgOverall.enableWayland;
      };

      environment.systemPackages = with pkgs; [
        gnome-tweaks
        gnome-extension-manager
        dconf2nix
        gnome-terminal ## Provides more functionalities than default gnome-console
        gnomeExtensions.tray-icons-reloaded
      ];
    })

    # X11 accessories
    (mkIf (cfg.enable && ! cfgOverall.enableWayland) {
      environment.systemPackages = with pkgs; [
        gnomeExtensions.x11-gestures
      ];
      services.touchegg.enable = true;
    })
  ];
}
