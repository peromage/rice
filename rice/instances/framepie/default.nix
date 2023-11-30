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
    "desktops/gnome.nix"
    "programs/gnupg.nix"
    "services/ssh.nix"
  ];

  rice = {
    hosts.hostName = "framepie";

    hardware = {
      secureBoot.enable = true;

      powerGovernor = {
        enable = true;
        profile = "powersave";
      };

      firmware.enable = true;
      peripherals.printing = true;
    };
  };
}
