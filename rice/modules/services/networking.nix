{ config, lib, rice, ... }:

let
  cfg = config.rice.services.networking;

  options = with lib; {
    enable = mkEnableOption "network management";
    enableBluetooth = mkEnableOption "Bluetooth management" // { default = true; };
  };

  librice = rice.lib;

in {
  options.rice.services.networking = options;

  config = with lib; librice.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        networking = {
          useDHCP = mkDefault true;
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
