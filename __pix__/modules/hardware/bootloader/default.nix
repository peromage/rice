{ config, lib, pix, ... }:

let
  cfg = config.pix.hardware.bootloader;
  libpix = pix.lib;

in {
  imports = with libpix; listDir isNotDefaultNix ./.;

  options.pix.hardware.bootloader = {};

  config = let
    enabledBootloaders = with lib; filterAttrs (_: config: config.enable) cfg;

  in with lib; {
    assertions = [
      {
        ## One or none
        assertion = length (attrNames enabledBootloaders) < 2;
        message = "Only one bootloader can be activated at a time.";
      }
    ];
  };
}
