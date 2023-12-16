{ config, lib, ... }:

let
  cfg = config.rice.services.firmware;

  options = with lib; {
    ## Don't forget `fwupdmgr update'
    enable = mkEnableOption "firmware management";
  };

in {
  options.rice.services.firmware = options;

  config = with lib; mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
