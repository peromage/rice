### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, rice, ... }:

let
  inherit (rice.flake.inputs) lanzaboote;
  librice = rice.lib;

in {
  imports = [
    ./boot.nix
    ./mounts.nix
    rice.dirs.modules
    lanzaboote.nixosModules.lanzaboote
  ];

  rice = {
    hosts.hostName = "Framepie";
    hosts.platform = "x86_64-linux";
    hosts.profiles.potpie.enable = true;
    users.profiles.fang.enable = true;
    users.root.enable = false;
    desktops.env.gnome.enable = true;

    services = {
      secureboot.enable = true;
      i18n.enable = true;
      networking.enable = true;
      firewall.enable = true;
      audio.enable = true;
      vconsole.enable = true;
      ssh.enable = true;
      documentation.enable = true;
      nix.enable = true;
      firmware.enable = true;
      peripherals.enable = true;
      ime.enabled = "fcitx";
      power = {
        enable = true;
        profile = "powersave";
      };
    };
  };
}
