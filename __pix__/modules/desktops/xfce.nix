{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = config.pix.desktops.env.xfce;

in {
  options.pix.desktops.env.xfce = with lib; {
    enable = mkEnableOption "XFCE";
    enableLightDM = lib.mkEnableOption "LightDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
