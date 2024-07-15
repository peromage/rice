{ config, lib, pix, ... }:

let
  cfg = config.pix.services.peripherals;

  options = with lib; {
    enable = mkEnableOption "peripheral management";
    enablePrinting = mkEnableOption "printing service" // { default = true; };
  };

in {
  options.pix.services.peripherals = options;

  config = lib.mkIf cfg.enable {
    services.printing.enable = cfg.enablePrinting;
  };
}
