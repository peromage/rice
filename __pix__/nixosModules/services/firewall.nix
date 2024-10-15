{ config, lib, ... }:

let
  cfg = config.pix.services.firewall;

in with lib; {
  options.pix.services.firewall = {
    enable = mkEnableOption "stateful firewall";

    allowedTCPPorts = mkOption {
      type = with types; listOf port;
      default = [];
      description = "Allowed TCP ports.";
    };

    allowedTCPPortRanges = mkOption {
      type = with types; listOf (attrsOf port);
      default = [];
      description = "Allowed TCP port range in attrs { from; to; }.";
    };

    allowedUDPPorts = mkOption {
      type = with types; listOf port;
      default = [];
      description = "Allowed UDP ports.";
    };

    allowedUDPPortRanges = mkOption {
      type = with types; listOf (attrsOf port);
      default = [];
      description = "Allowed UDP port range in attrs { from; to; }.";
    };
  };

  config = mkIf cfg.enable {
    ## Explicitly disable nftables to use iptables instead for better compatibility
    networking.nftables.enable = false;

    networking.firewall = {
      enable = true;
      rejectPackets = false; # Maybe ignore
      allowPing = true; # Maybe refuse
      autoLoadConntrackHelpers = false;

      ## Logging
      checkReversePath = true; # Restrict responses via the same interface
      logReversePathDrops = false;
      logRefusedUnicastsOnly = true;
      logRefusedPackets = false; # There would be a lot log if enabled
      logRefusedConnections = true;

      ## Port rules
      allowedTCPPorts = cfg.allowedTCPPorts;
      allowedTCPPortRanges = cfg.allowedTCPPortRanges;
      allowedUDPPorts = cfg.allowedUDPPorts;
      allowedUDPPortRanges = cfg.allowedUDPPortRanges;
    };
  };
}
