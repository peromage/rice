{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    pix.nixosModules.default
  ];

  pix = {
    hosts.profiles.PROX.enable = true;
    users.profiles.wangguan.enable = true;
    users.profiles.root.enable = false;
  };
}
