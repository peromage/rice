{ config, lib, pkgs, ... }:

let
  name = "PROX";
  cfg = config.pix.hosts.profiles.${name};

in with lib; {
  options.pix.hosts.profiles.${name} = {
    enable = mkEnableOption "basic host config";
  };

  config = mkIf cfg.enable {
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
      pixPkgs.emacs

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
