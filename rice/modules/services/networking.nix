{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.services.networking;

in with lib; {
  options.rice.services.networking = {
    enable = mkEnableOption "network management";
    enableWireless = mkEnableOption "wireless management" // { default = true; };
    enableBluetooth = mkEnableOption "Bluetooth management" // { default = true; };
  };

  config = librice.mkMergeIf [
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
          };
        };
      };
    }

    {
      cond = cfg.enable && cfg.enableWireless;
      as = {
        networking = {
          wireless.enable = mkDefault true;
          networkmanager.wifi = {
            backend = "wpa_supplicant";
            powersave = true;
            scanRandMacAddress = true;
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
