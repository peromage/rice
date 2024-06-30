### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, rice, ... }:

{
  imports = [
    ./boot.nix
    ./mounts.nix
    rice.nixosModules.default
  ];

  rice = {
    hosts.hostName = "Framepie";
    hosts.platform = "x86_64-linux";
    hosts.profiles.potpie.enable = true;
    users.profiles.fang.enable = true;
    users.root.enable = false;
    desktops.env.gnome.enable = true;

    services = {
      boot.enabled = "secure-boot";
      i18n.enable = true;
      networking.enable = true;
      firewall.enable = true;
      audio.enable = true;
      vconsole.enable = true;
      ssh.enable = true;
      documentation.enable = true;
      nix.enable = true;
      firmware.enable = true;
      peripherals.enable = true;
      ime.enabled = "fcitx";
      power = {
        enable = true;
        profile = "powersave";
      };
      steam = {
        enable = true;
        openFirewall.remotePlay = true;
      };
      libvirtd.enable = true;
    };
  };
}
