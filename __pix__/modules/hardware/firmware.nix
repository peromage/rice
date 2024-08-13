{ config, lib, ... }:

let
  cfg = config.pix.hardware.firmware;

  options = {
    ## Don't forget `fwupdmgr update'
    enable = lib.mkEnableOption "firmware management";
  };

in {
  options.pix.hardware.firmware = options;

  config = lib.mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
