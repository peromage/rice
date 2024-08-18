{ config, lib, pix, ... }:

let
  libpix = pix.lib;
  cfg = config.pix.hosts;

in {
  imports = with libpix; listDir isNotDefaultNix ./.;

  /* Interface */
  options.pix.hosts = with lib; {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host name for this machine.
        For clarification, this needs to be specified explicitly.
      '';
    };

    };

    ## Each profile can have their own options, while `enable' is mandatory
    profiles = {};
  };

  /* Implementation */
  config = lib.mkIf (libpix.anyEnable cfg.profiles) {
    ## Common
    networking.hostName = cfg.hostName;

    ## Assertions
    assertions = [
      {
        assertion = cfg.hostName != null;
        message = ''
              No hostname provided.
              Either `pix.hosts.hostName' is not set or no host profile is enabled."
            '';
      }
    ];
  };
}
