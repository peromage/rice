{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.rice.services.ssh;

  options = {
    enable = mkEnableOption "SSH service";
    enablePassword = mkEnableOption "SSH password login";
  };

in {
  options.rice.services.ssh = options;

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
