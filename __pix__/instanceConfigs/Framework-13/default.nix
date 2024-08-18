### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    ./mounts.nix
    pix.nixosModules.default
  ];

  pix = {
    hosts.hostName = "PRMG";
    hosts.platform = "x86_64-linux";
    hosts.profiles.dailyDriver.enable = true;
    users.profiles.fang.enable = true;
    users.profiles.root.enable = false;
  };
}
