{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.hardware.networking;

  options = with lib; {
    enable = mkEnableOption "network management";
  };

in {
  options.pix.hardware.networking = options;

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
