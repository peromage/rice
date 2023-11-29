{ config, lib, ... }:

let
  cfg = config.rice.hardware.firmware;

in with lib; {
  options.rice.hardware.firmware = {
    enable = mkEnableOption "Enable firmware management. Don't forget `fwupdmgr update'.";
  };

  config = mkIf cfg.enable {
    services.fwupd.enable = true;
  };
}
