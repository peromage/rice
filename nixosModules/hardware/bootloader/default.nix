{ config, lib, pix, ... }:

let
  cfg = config.pix.hardware.bootloader;
  libpix = pix.lib;

in with lib; {
  imports = with libpix; listDir isNotDefaultNix ./.;

  options.pix.hardware.bootloader = {};

  config = let
    enabledBootloaders = filterAttrs (_: config: config.enable) cfg;

  in {
    assertions = [
      {
        ## One or none
        assertion = length (attrNames enabledBootloaders) < 2;
        message = "Only one bootloader can be activated at a time.";
      }
    ];
  };
}
