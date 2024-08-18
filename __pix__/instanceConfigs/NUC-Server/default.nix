{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    pix.nixosModules.default
  ];

  pix = {
    hosts.hostName = "LADDER";
    hosts.platform = "x86_64-linux";
    hosts.profiles.basic.enable = true;
    users.profiles.wangguan.enable = true;
    users.profiles.root.enable = false;
  };
}
