{ mkDesktopOptions, ... }:
{ config, lib, ... }:

let
  cfgAll = config.pix.desktops;
  cfg = config.pix.desktops.env.xfce;

  options = mkDesktopOptions {
    name = "XFCE";
  } // {
    enableLightDM = lib.mkEnableOption "LightDM display manager" // { default = true; };
  };

in {
  options.pix.desktops.env.xfce = options;

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
