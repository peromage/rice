{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hosts.profiles.basic;

in {
  options.pix.hosts.profiles.basic = with lib; {
    enable = mkEnableOption "basic host config";
  };

  config = lib.mkIf cfg.enable {
    /* Pix options */
    pix.services = {
      i18n.enable = true;
      firewall.enable = true;
      vconsole.enable = true;
      sshd.enable = true;
      nix.enable = true;
    };

    pix.desktops.env.xfce.enable = true;

    environment.systemPackages = with pkgs; [
      brave
      vim
      tmux
      git
      curl
      wget
      rsync
      tree

      ## Filesystems
      ntfs3g
      exfat
      exfatprogs
      e2fsprogs
      fuse
      fuse3
    ];
  };
}
