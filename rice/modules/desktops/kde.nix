{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.kde;
  cfgAll = config.rice.desktops;

in with lib; {
  options.rice.desktops.env.kde = {
    enable = mkEnableOption "KDE Plasma desktop environment";
    disableSDDM = mkEnableOption "SDDM off";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = !cfg.disableSDDM;
      displayManager.sddm.wayland.enable = cfgAll.enableWayland;
    };
  };
}
