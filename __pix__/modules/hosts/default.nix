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
        This takes precedence over the name defined in each host profile.
        If this is not defined, the name of last enabled host profile will be
        used.
      '';
    };

    platform = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host platform architecture.
        For clarification, this needs to be specified explicitly.
      '';
    };

    ## Each profile can have their own options, while `enable' is mandatory.
    profiles = {};
  };

  /* Implementation */
  config = {
    ## Common
    nixpkgs.hostPlatform = cfg.platform;
    networking.hostName = cfg.hostName;

    ## Assertions
    assertions = [
      {
        assertion = cfg.platform != null;
        message = "Platform has be explicitly specified.";
      }

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
