{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfg = config.rice.desktops.env.xfce;
  cfgAll = config.rice.desktops;

in with lib; {
  options.rice.desktops.env.xfce = mkDesktopOptions {
    name = "XFCE";
  } // (with lib; {
    enableLightDM = mkEnableOption "LightDM display manager" // { default = true; };
  });

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
