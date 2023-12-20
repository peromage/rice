{ config, lib, ... }:

let
  inherit (lib) types mkEnableOption mkOption mkIf singleton;

  cfg = config.rice.services.shadowsocks;

  options = {
    enable = mkEnableOption "ShadowSocks";

    port = mkOption {
      type = with types; nullOr port;
      default = 8388;
      description = "Default service port.";
    };

    bind = mkOption {
      type = with types; listOf str;
      default = [ "0.0.0.0" ];
      description = "Addresses to listen to.";
    };

    password = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Connection password.";
    };

    extra = mkOption {
      type = types.attrs;
      default = {};
      description = "Extra configurations in keys and values.";
    };
  };

in {
  options.rice.services.shadowsocks = options;

  config = mkIf cfg.enable {
    assertions = singleton {
      assertion = null != cfg.password;
      message = "No password specified for service Shadowsocks.";
    };

    services.shadowsocks = {
      enable = true;
      password = cfg.password;
      port = cfg.port;
      localAddress = cfg.bind;
      extraConfig = cfg.extra;
      encryptionMethod = "chacha20-ietf-poly1305";
      fastOpen = true;
      mode = "tcp_and_udp";
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];
  };
}
