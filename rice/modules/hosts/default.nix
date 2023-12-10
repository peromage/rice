### Host options

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts;

  enabledHosts = lib.filterAttrs (n: v: v.enable) cfg.hosts;
  /* Handle host name.
     The precedence of the host name specified in options is as follow:
       1. hosts.hostName
       2. hosts.<host>

     Any one of them must be specified.
     If `hosts.hostName' exists, the rest of the options will be ignored.
     If no global hostName exists, the last host name in the set (in alphabetic
     order) will be used.
    */
  finalHostName = librice.either
    cfg.hostName
    (lib.foldlAttrs
      (a: n: v: librice.either n a)
      null
      enabledHosts);

in with lib; {
  imports = librice.allButDefault ./.;

  options.rice.hosts = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name for this machine.";
    };

    hosts = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          enable = mkEnableOption "Host activation";

          name = mkOption {
            type = str;
            description = "Host name";
          };
        };
      });
      description = "List of host configs.";
    };
  };

  config = {
    networking.hostName = (assert null != finalHostName; finalHostName);
  };
}
