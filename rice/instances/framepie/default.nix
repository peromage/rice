### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ rice, ... }:

let
  librice = rice.lib;

in
librice.buildNixOS "x86_64-linux" {
  imports = [
    ./boot.nix
    ./mounts.nix
  ] ++ librice.getModules [
    "options"
    "hosts/potpie"
    "users/fang"
    "desktops/gnome.nix"
    "programs/gnupg.nix"
    "services/firmware.nix"
    "services/peripherals.nix"
    "services/ssh.nix"
  ];

  rice = {
    host.name = "framepie";
    boot.secureboot.enable = true;
    power = {
      enable = true;
      profile = "powersave";
    };
  };
}
