{ config, lib, ... }:

let
  cfg = config.rice.hardware.firmware;

in with lib; {
  options.rice.hardware.firmware = {
    ## Don't forget `fwupdmgr update'
    enable = mkEnableOption "firmware management";
  };

  config = mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
