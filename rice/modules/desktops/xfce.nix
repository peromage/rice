{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.xfce;

  options = with lib; mkDesktopOptions {
    name = "XFCE";
  } // {
    enableLightDM = mkEnableOption "LightDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.xfce = options;

  config = with lib; mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
