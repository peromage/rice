{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.xfce;

in with lib; {
  options.pix.desktops.env.xfce = {
    enable = mkEnableOption "XFCE";
    enableLightDM = mkEnableOption "LightDM display manager" // { default = true; };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
