{ config, lib, pix, ... }:

let
  cfg = config.pix.hardware.peripherals;

  options = with lib; {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

in {
  options.pix.hardware.peripherals = options;

  config = lib.mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
