{ config, lib, rice, ... }:

let
  cfg = config.rice.services.peripherals;

  options = with lib; {
    printing = mkEnableOption "printing service";
  };

  librice = rice.lib;

in {
  options.rice.services.peripherals = options;

  config = with lib; librice.mkMergeIf [
    {
      cond = cfg.printing;
      as = {
        services.printing.enable = true;
      };
    }
  ];
}
