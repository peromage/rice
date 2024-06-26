{ lib, rice, ... }:

{
  imports = [
    ./accounts.nix
    ./hardware.nix
    rice.paths.modules
  ];

  rice = {
    hosts.hostName = "Chicken65";
    hosts.platform = "x86_64-linux";
    hosts.profiles.biryani.enable = true;
    desktops.env.xfce.enable = true;

    services = {
      i18n.enable = true;
      networking.enable = true;
      firewall.enable = true;
      audio.enable = true;
      vconsole.enable = true;
      ssh.enable = true;
      nix.enable = true;
    };
  };
}
