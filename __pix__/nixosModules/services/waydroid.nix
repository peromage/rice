{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.waydroid;

in with lib; {
  options.pix.services.waydroid = {
    enable = mkEnableOption "waydroid";
  };

  config = mkIf cfg.enable {
    virtualisation.waydroid.enable = true;

    /* To start a user session in the background and avoid accidental shutdown
       If don't care about the session life time, use `waydroid show-full-ui' to
       directly start it.
    */
    systemd.user.services.waydroid-session = {
      enable = true;
      description = "Waydroid user session";
      after = [ "waydroid-container.service" ];
      # wantedBy = [ "default.target" ]; # Means to be started manually on demand
      serviceConfig = {
        Type = "simple";
        Restart = "no";
        ExecStart = "${pkgs.waydroid}/bin/waydroid session start"; # Assume the option `virtualisation.waydroid' uses the same package
      };
    };

    environment.systemPackages = with pkgs; [
      ## Helper scripts
      (writeScriptBin "start-waydroid-session.sh" ''
        systemctl --user is-active --quiet waydroid-session.service || systemctl --user start waydroid-session.service
        waydroid show-full-ui
      '')
      (writeScriptBin "stop-waydroid-session.sh" ''
        systemctl --user stop waydroid-session.service
      '')
    ];
  };
}
