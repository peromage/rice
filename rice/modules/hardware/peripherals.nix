{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hardware.peripherals;

in with lib; {
  options.rice.hardware.peripherals = {
    printing = mkEnableOption "Enable printing service";
  };

  config = librice.mkMergeTopLevelCond ["services"] [
    {
      ## Printing service
      cond = cfg.printing;
      as = {
        services.printing.enable = true;
      };
    }];
}
