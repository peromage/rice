{ config, lib, ... }:

let
  cfg = config.rice.services.ssh;
in {
  config = lib.mkIf cfg.enable {
    services.openssh = {
      enable = true;
      ports = [ 22 ];
      openFirewall = true; # Whitelist the ports
      settings = {
        X11Forwarding = false;
        PermitRootLogin = false;
        PasswordAuthentication = false;
      };
    };
  };
}
