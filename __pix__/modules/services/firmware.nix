{ config, lib, ... }:

let
  cfg = config.rice.services.firmware;

  options = {
    ## Don't forget `fwupdmgr update'
    enable = lib.mkEnableOption "firmware management";
  };

in {
  options.rice.services.firmware = options;

  config = lib.mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
