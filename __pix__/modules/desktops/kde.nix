{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.profiles.kde;

in {
  options.pix.desktops.profiles.kde = with lib; {
    enable = mkEnableOption "KDE";
    enableSDDM = mkEnableOption "SDDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgOverall.enableWayland;
    };
  };
}
