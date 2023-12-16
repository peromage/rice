{ config, lib, rice, ... }:

let
  cfg = config.rice.services.peripherals;

  options = with lib; {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

  librice = rice.lib;

in {
  options.rice.services.peripherals = options;

  config = with lib; mkIf cfg.enable {
    services.printing.enable = enablePrinting;
  };
}
