### Host options

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts;

  /* Additional arguments to import submodules.

     CONTRACT: Each profile declared in this set must have options:

       - enable
       - name
  */
  args = {
    mkProfileOptions = { name }: with lib; {
      enable = mkEnableOption "Host ${name} activation";

      name = mkOption {
        type = types.str;
        default = name;
        description = "Host name for ${name}";
      };
    };
  };

  enabledHosts = lib.filterAttrs (n: v: v.enable) cfg.profiles;

  /* Handle host name.
     The precedence of the host name specified in options is as follow:
       1. hosts.hostName
       2. hosts.<profile>.name
       3. hosts.<profile>

     Any one of them must be specified.
     If `hosts.hostName' exists, the rest of the options will be ignored.
     If no global hostName exists, the last host name in the set (in alphabetic
     order) will be used.
    */
  finalHostName = librice.either
    cfg.hostName
    (lib.foldlAttrs
      (a: n: v: librice.either v.name n)
      null
      enabledHosts);

in with lib; {
  imports = with librice; callListWithArgs args (allButDefault ./.);

  options.rice.hosts = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name for this machine.";
    };

    profiles = {};
  };

  config = {
    networking.hostName = (assert null != finalHostName; finalHostName);
  };
}
