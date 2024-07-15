{ config, lib, rice, ... }:

let
  cfg = config.rice.services.peripherals;

  options = with lib; {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

in {
  options.rice.services.peripherals = options;

  config = lib.mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
