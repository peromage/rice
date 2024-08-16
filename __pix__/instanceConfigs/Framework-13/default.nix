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
    desktops.env.gnome.enable = true;

    services = {
      i18n.enable = true;
      firewall.enable = true;
      vconsole.enable = true;
      ssh.enable = true;
      documentation.enable = true;
      nix.enable = true;
      ime.enabled = "fcitx";
      steam = {
        enable = true;
        openFirewall.remotePlay = true;
      };
      libvirtd.enable = true;
    };
  };
}
