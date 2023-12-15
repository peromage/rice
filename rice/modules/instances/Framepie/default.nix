### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, rice, ... }:

let
  librice = rice.lib;

in {
  imports = [
    ./boot.nix
    ./mounts.nix
    rice.dirs.modules
  ];

  rice = {
    hosts.hostName = "Framepie";
    hosts.platform = "x86_64-linux";
    hosts.profiles.potpie.enable = true;
    users.profiles.fang.enable = true;
    users.root.enable = false;
    desktops.env.gnome.enable = true;

    hardware = {
      secureBoot.enable = true;

      powerGovernor = {
        enable = true;
        profile = "powersave";
      };

      firmware.enable = true;
      peripherals.printing = true;
    };

    services = {
      ssh.enable = true;
      ime.enabled = "fcitx";
      documentation.enable = true;
      nix.enable = true;
      firewall.enable = true;
    };
  };
}
