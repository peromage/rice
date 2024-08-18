{ config, lib, ... }:

let
  cfg = config.pix.hardware.peripherals;

in with lib; {
  options.pix.hardware.peripherals = {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

  config = mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
