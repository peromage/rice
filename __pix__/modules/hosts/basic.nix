{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hosts.profiles.basic;

in {
  options.pix.hosts.profiles.basic = with lib; {
    enable = mkEnableOption "basic host config";
  };

  config = lib.mkIf cfg.enable {
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
