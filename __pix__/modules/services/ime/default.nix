{ config, lib, pix, ... }:

let
  cfg = config.pix.services.ime;
  libpix = pix.lib;

in {
  imports = with libpix; listDir isNotDefaultNix ./.;

  options.pix.services.ime = {};

  config = let
    enabledIME = with lib; filterAttrs (_: config: config.enable) cfg;

  in with lib; {
    assertions = [
      {
        assertion = length (attrNames enabledIME) < 2;
        message = "Only one IME can be activated at a time.";
      }
    ];
  };
}
