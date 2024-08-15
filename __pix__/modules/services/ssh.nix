{ config, lib, ... }:

let
  cfg = config.pix.services.ssh;

in {
  options.pix.services.ssh = with lib; {
    enable = mkEnableOption "SSH service";
    enablePassword = mkEnableOption "SSH password login";
  };

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
