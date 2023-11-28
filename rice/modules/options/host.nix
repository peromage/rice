### Host options

{ config, lib, ... }:

with lib;
let
  cfg = config.rice;

in {
  options.rice.host = {
    name = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name";
    };
  };

  config = {
    networking.hostName = mkIf (null != cfg.host.name) cfg.host.name;
  };
}
