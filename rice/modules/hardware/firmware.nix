{ config, lib, ... }:

let
  cfg = config.rice.hardware.firmware;

in with lib; {
  options.rice.hardware.firmware = {
    ## Don't forget `fwupdmgr update'
    enable = mkEnableOption "Firmware management";
  };

  config = mkIf cfg.enable {
    services.fwupd.enable = true;
  };
}
