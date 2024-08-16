{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hosts.profiles.biryani;

in {
  options.pix.hosts.profiles.biryani = with lib; {
    enable = mkEnableOption "host biryani";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      firefox
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
