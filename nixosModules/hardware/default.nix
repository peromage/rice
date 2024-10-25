### All service options

{ config, lib, pix, ... }:

let
  cfg = config.pix.hardware;

in with lib; {
  imports = with pix.lib; listDir isNotDefaultNix ./.;

  options.pix.hardware = {
    platform = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host platform architecture.
        For clarification, this needs to be specified explicitly.
      '';
    };
  };

  config = {
    nixpkgs.hostPlatform = cfg.platform;

    assertions = singleton {
      assertion = cfg.platform != null;
      message = "Platform must be explicitly specified.";
    };
  };
}
