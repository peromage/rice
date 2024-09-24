{ config, lib, pix, ... }:

let
  cfg = config.pix.hosts;
  libpix = pix.lib;

in with lib; {
  imports = with libpix; listDir isNotDefaultNix ./.;

  /* Interface */
  options.pix.hosts = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host name for this machine.
        This is default to the host profile name.  However, this option can be
        used to override it.
        NOTE: This only has effect when one of the host profiles is enabled.
      '';
    };

    ## Each profile can have their own options, while `enable' is mandatory
    profiles = {};
  };

  /* Implementation */
  config = let
    enabledHosts = filterAttrs (_: config: config.enable) cfg.profiles;
    numberOfEnabledHosts = length (attrNames enabledHosts);

  in mkIf (numberOfEnabledHosts > 0) {
    ## There is only one name if condition is right
    pix.hosts.hostName = mkDefault (head (attrNames enabledHosts));

    networking.hostName = cfg.hostName;

    ## Assertions
    assertions = singleton {
      assertion = numberOfEnabledHosts == 1;
      message = "Only one host can be enabled at a time.";
    };
  };
}
