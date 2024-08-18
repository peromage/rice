{ config, lib, ... }:

let
  cfg = config.pix.hardware.networking;

in with lib; {
  options.pix.hardware.networking = {
    enable = mkEnableOption "network management";
  };

  config = mkIf cfg.enable {
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
