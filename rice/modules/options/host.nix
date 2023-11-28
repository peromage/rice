### Host options

{ config, lib, ... }:

with lib;
let
  cfg = config.rice.host;

in {
  options.rice.host = {
    name = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name";
    };
  };

  config = {
    networking.hostName = mkIf (null != cfg.name) cfg.name;
  };
}
