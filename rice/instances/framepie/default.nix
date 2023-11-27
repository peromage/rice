### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, rice, ... }:

let
  librice = rice.lib;

in {
  networking.hostName = "framepie";

  imports = [
    ./boot.nix
    ./mount.nix
  ] ++ librice.getOSModules [
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
