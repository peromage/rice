{ config, lib, ... }:

let
  cfg = config.rice.services.bluetooth;

  options = with lib; {
    enable = mkEnableOption "Bluetooth management";
  };

in {
  options.rice.services.bluetooth = options;

  config = lib.mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
