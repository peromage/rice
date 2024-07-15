{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.hosts;

  options = {
    hostName = with lib; mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name for this machine.";
    };

    platform = with lib; mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host platform architecture.";
    };

    profiles = {};
  };

  /* Additional arguments to import submodules.

     CONTRACT: Each profile declared in this set must have options:

       - enable
       - name
  */
  args = {
    mkProfileOptions = { name }: with lib; {
      enable = mkEnableOption "host";

      name = mkOption {
        type = types.str;
        default = name;
        description = "Host name.";
      };
    };
  };

  ## Host config is only enabled if any one of the profiles is turned on
  enableHostConfig = libpix.anyEnable cfg.profiles;
  enabledHosts = libpix.filterEnable cfg.profiles;

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
  finalHostName = with libpix; either
    cfg.hostName
    (lib.foldlAttrs
      (a: n: v: either v.name n)
      null
      enabledHosts);

in {
  imports = with libpix; callAll args (listDir isNotDefaultNix ./.);
  options.pix.hosts = options;

  config = lib.mkIf enableHostConfig {
    assertions = [
      {
        assertion = null != cfg.platform;
        message = "No platform specified.";
      }

      {
        assertion = null != finalHostName;
        message = "No hostname provided.";
      }
    ];

    nixpkgs.hostPlatform = cfg.platform;
    networking.hostName = finalHostName;
  };
}
