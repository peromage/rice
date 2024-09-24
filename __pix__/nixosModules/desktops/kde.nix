{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.kde;

in with lib; {
  options.pix.desktops.env.kde = {
    enable = mkEnableOption "KDE";
    enableSDDM = mkEnableOption "SDDM display manager" // { default = true; };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgOverall.enableWayland;
    };
  };
}
