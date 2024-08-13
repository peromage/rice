{ lib, pix, ... }:

{
  imports = [
    ./accounts.nix
    ./hardware.nix
    pix.nixosModules.default
  ];

  pix = {
    hosts.hostName = "Chicken65";
    hosts.platform = "x86_64-linux";
    hosts.profiles.biryani.enable = true;
    desktops.env.xfce.enable = true;

    services = {
      i18n.enable = true;
      firewall.enable = true;
      vconsole.enable = true;
      ssh.enable = true;
      nix.enable = true;
    };
  };
}
