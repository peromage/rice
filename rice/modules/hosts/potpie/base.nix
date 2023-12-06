{ lib, rice, ... }:

let
  librice = rice.lib;

in {
  ## Sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  ## Network
  networking = {
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
    firewall.enable = true;
  };
}
