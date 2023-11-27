{ lib, rice, ... }:

let
  librice = rice.lib;

in
{
  networking.hostName = "framepie";

  imports = librice.getOSModules [
    "hardware/framework-12th-gen-intel"
    "hosts/potpie"
    "users/fang"
    "common/nix-settings.nix"
    "common/locale.nix"
    "common/fonts.nix"
    "common/packages.nix"
    "desktops/gnome.nix"
    "programs/gnupg.nix"
    "services/firmware.nix"
    "services/peripherals.nix"
    "services/ssh.nix"
  ];
}
