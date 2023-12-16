{ config, lib, ... }:

let
  cfg = config.rice.services.ssh;

in with lib; {
  options.rice.services.ssh = {
    enable = mkEnableOption "SSH service";
    enablePassword = mkEnableOption "SSH password login";
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      ports = [ 22 ];
      openFirewall = true; # Whitelist the ports
      settings = {
        X11Forwarding = false;
        PermitRootLogin = "no";
        PasswordAuthentication = cfg.enablePassword;
      };
    };
  };
}
