{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.services.peripherals;

in with lib; {
  options.rice.services.peripherals = {
    printing = mkEnableOption "printing service";
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
