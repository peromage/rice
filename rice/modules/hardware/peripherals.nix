{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hardware.peripherals;

in with lib; {
  options.rice.hardware.peripherals = {
    printing = mkEnableOption "Printing service.";
  };

  config = librice.mkMergeIf [
    {
      cond = cfg.printing;
      as = {
        services.printing.enable = true;
      };
    }
  ];
}
