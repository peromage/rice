{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfgAll = config.rice.desktops;
  cfg = config.rice.desktops.env.xfce;

  options = mkDesktopOptions {
    name = "XFCE";
  } // {
    enableLightDM = mkEnableOption "LightDM display manager" // { default = true; };
  };

in {
  options.rice.desktops.env.xfce = options;

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
