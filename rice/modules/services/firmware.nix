{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.rice.services.firmware;

  options = {
    ## Don't forget `fwupdmgr update'
    enable = mkEnableOption "firmware management";
  };

in {
  options.rice.services.firmware = options;

  config = mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
