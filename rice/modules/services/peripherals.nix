{ config, lib, rice, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.rice.services.peripherals;

  options = {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

in {
  options.rice.services.peripherals = options;

  config = mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
