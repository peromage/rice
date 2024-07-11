{ config, lib, rice, ... }:

let
  librice = rice.lib;

  cfg = config.rice.services.networking;

  options = with lib; {
    enable = mkEnableOption "network management";
  };

in {
  options.rice.services.networking = options;

  config = lib.mkIf cfg.enable {
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
