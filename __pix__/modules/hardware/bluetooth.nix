{ config, lib, ... }:

let
  cfg = config.pix.hardware.bluetooth;

in with lib; {
  options.pix.hardware.bluetooth = {
    enable = mkEnableOption "Bluetooth management";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
