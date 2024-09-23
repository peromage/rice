{ config, lib, ... }:

let
  cfg = config.pix.services.waydroid;

in with lib; {
  options.pix.services.waydroid = {
    enable = mkEnableOption "waydroid";
  };

  config = mkIf cfg.enable {
    virtualisation.waydroid.enable = true;

    systemd.user.services.waydroid-session = {
      enable = false; # Means to be started manually on demand
      description = "Waydroid user session";
      after = [ "waydroid-container.service" ];
      serviceConfig = {
        Type = "simple";
        Restart = "no";
        ExecStart = "/usr/bin/env waydroid session start";
      };
    };
  };
}
