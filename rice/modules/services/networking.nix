{ config, lib, rice, ... }:

let
  librice = rice.lib;

  cfg = config.rice.services.networking;

  options = with lib; {
    enable = mkEnableOption "network management";
    enableBluetooth = mkEnableOption "Bluetooth management" // { default = true; };
  };

in {
  options.rice.services.networking = options;

  config = librice.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        networking = {
          useDHCP = lib.mkDefault true;
          useHostResolvConf = false;
          networkmanager = {
            enable = true;
            dns = "default";
            dhcp = "internal";
            wifi = {
              backend = "wpa_supplicant";
              powersave = true;
              scanRandMacAddress = true;
            };
          };
        };
      };
    }

    {
      cond = cfg.enable && cfg.enableBluetooth;
      as = {
        hardware.bluetooth = {
          enable = true;
          powerOnBoot = true;
        };
      };
    }
  ];
}
