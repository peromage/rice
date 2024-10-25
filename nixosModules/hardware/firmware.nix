{ config, lib, ... }:

let
  cfg = config.pix.hardware.firmware;

in {
  options.pix.hardware.firmware = {
    ## Don't forget `fwupdmgr update'
    enable = lib.mkEnableOption "firmware management";
  };

  config = lib.mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
