{ config, lib, ... }:

let
  cfg = config.pix.hardware.peripherals;

in {
  options.pix.hardware.peripherals = with lib; {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
