### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ rice, ... }:

let
  librice = rice.lib;

in
librice.buildNixOS "x86_64-linux" {
  imports = [
    ./boot.nix
    ./mounts.nix
    librice.moduleTopLevel
  ] ++ librice.getModules [
    "hosts/potpie"
    "users/fang"
    "programs/gnupg.nix"
  ];

  rice = {
    hosts.hostName = "framepie";

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
    };
  };
}
