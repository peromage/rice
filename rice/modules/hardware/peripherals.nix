{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hardware.peripherals;

in with lib; {
  options.rice.hardware.peripherals = {
    printing = mkEnableOption "Printing service.";
  };

  config = mkMerge [
    (mkIf cfg.printing {
      services.printing.enable = true;
    })
  ];
}
