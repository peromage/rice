{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    pix.nixosModules.default
  ];

  system.stateVersion = "24.05";

  pix = {
    hosts.profiles.PROX.enable = true;
    users.profiles.wangguan.enable = true;
    users.profiles.root.enable = false;
  };
}
