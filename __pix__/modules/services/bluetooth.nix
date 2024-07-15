{ config, lib, ... }:

let
  cfg = config.pix.services.bluetooth;

  options = with lib; {
    enable = mkEnableOption "Bluetooth management";
  };

in {
  options.pix.services.bluetooth = options;

  config = lib.mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
