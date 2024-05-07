{ config, lib, librice, ... }:

let
  inherit (lib) mkEnableOption mkDefault;
  inherit (librice) mkMergeIf;

  cfg = config.rice.services.networking;

  options = {
    enable = mkEnableOption "network management";
    enableBluetooth = mkEnableOption "Bluetooth management" // { default = true; };
  };

in {
  options.rice.services.networking = options;

  config = mkMergeIf [
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
