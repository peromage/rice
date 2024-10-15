{ config, lib, ... }:

let
  cfg = config.pix.services.frp;

in with lib; {
  options.pix.services.frp = {
    enable = mkEnableOption "FRP server";

    bindAddr = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "The address that the server listens to.";
    };

    bindPort = mkOption {
      type = types.port;
      default = 7000;
      description = "The port for server and clients communication.";
    };

    openPorts = mkOption {
      type = with types; listOf port;
      default = [];
      description = "Additional ports that are allowed by firewall.";
    };

    password = mkOption {
      type = types.str;
      default = "P@55w0rd";
      description = "Token for client connection authentication.";
    };
  };

  config = let
    allowedPorts = cfg.openPorts ++ [ cfg.bindPort ];

  in mkIf cfg.enable {
    services.frp = {
      enable = true;
      role = "server";
      settings = {
        bindAddr = cfg.bindAddr;
        bindPort = cfg.bindPort;
        auth.method = "token";
        auth.token = cfg.password;
        maxPortsPerClient = 0;
      };
    };

    pix.services.firewall = {
      allowedTCPPorts = allowedPorts;
      allowedUDPPorts = allowedPorts;
    };
  };
}
