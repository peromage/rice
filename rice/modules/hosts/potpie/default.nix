{ lib, rice, ... }:

let
  librice = rice.lib;

in {
  imports = librice.allButDefault ./.;

  ## Host name
  networking.hostName = lib.mkDefault "potpie";

  ## System state version
  system.stateVersion = "23.05";

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
