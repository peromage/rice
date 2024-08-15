{ config, lib, ... }:

let
  cfg = config.pix.hardware.networking;

in {
  options.pix.hardware.networking = with lib; {
    enable = mkEnableOption "network management";
  };

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
