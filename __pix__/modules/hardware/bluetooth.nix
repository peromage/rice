{ config, lib, ... }:

let
  cfg = config.pix.hardware.bluetooth;

  options = with lib; {
    enable = mkEnableOption "Bluetooth management";
  };

in {
  options.pix.hardware.bluetooth = options;

  config = lib.mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
