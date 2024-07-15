{ config, lib, ... }:

let
  cfg = config.pix.services.ssh;

  options = with lib; {
    enable = mkEnableOption "SSH service";
    enablePassword = mkEnableOption "SSH password login";
  };

in {
  options.pix.services.ssh = options;

  config = lib.mkIf cfg.enable {
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
