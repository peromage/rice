### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ rice, ... }:

let
  librice = rice.lib;

in
librice.buildNixOS "x86_64-linux" {
  networking.hostName = "framepie";

  imports = [
    ./boot.nix
    ./mount.nix
  ] ++ librice.getModules [
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
