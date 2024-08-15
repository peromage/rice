{ config, lib, ... }:

let
  cfg = config.pix.services.firewall;

  tcpPreset = [

  ];

  tcpRangePreset = [

  ];

  udpPreset = [

  ];

  udpRangePreset = [

  ];

  combinedRules = if cfg.enablePreset then {
    allowedTCPPorts = tcpPreset ++ cfg.tcp;
    allowedTCPPortRanges = tcpRangePreset ++ cfg.tcpRange;
    allowedUDPPorts = udpPreset ++ cfg.udp;
    allowedUDPPortRanges = udpRangePreset ++ cfg.udpRange;
  } else {
    allowedTCPPorts = cfg.tcp;
    allowedTCPPortRanges = cfg.tcpRange;
    allowedUDPPorts = cfg.udp;
    allowedUDPPortRanges = cfg.udpRange;
  };

in {
  options.pix.services.firewall = with lib; {
    enable = mkEnableOption "stateful firewall";
    enablePreset = mkEnableOption "preset TCP/UDP rules" // { default = true; };

    tcp = mkOption {
      type = with types; listOf port;
      default = [];
      description = "Allowed TCP ports.";
    };

    tcpRange = mkOption {
      type = with types; listOf (attrsOf port);
      default = [];
      description = "Allowed TCP port ranges.";
    };

    udp = mkOption {
      type = with types; listOf port;
      default = [];
      description = "Allowed UDP ports.";
    };

    udpRange = mkOption {
      type = with types; listOf (attrsOf port);
      default = [];
      description = "Allowed UDP port ranges.";
    };
  };

  config = lib.mkIf cfg.enable {
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
    } // combinedRules;
  };
}
