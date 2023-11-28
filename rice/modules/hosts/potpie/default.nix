{ config, lib, pkgs, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts.potpie;

in {
  imports = librice.allButDefault ./.;

  options.rice.hosts.potpie = librice.mkHostPreset "Potpie";

  config = librice.mkHostPresetConfig cfg {};
}
