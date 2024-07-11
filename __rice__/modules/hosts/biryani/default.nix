{ mkProfileOptions, ... }:
{ config, lib, pkgs, ... }:

let
  cfg = config.rice.hosts.profiles.biryani;

  options = mkProfileOptions {
    name = "biryani";
  };

in {
  options.rice.hosts.profiles.biryani = options;

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
