{ mkProfileOptions, ... }:
{ config, lib, pkgs, ... }:

let
  cfg = config.rice.hosts.profiles.biryani;

  options = with lib; mkProfileOptions {
    name = "biryani";
  };

in {
  options.rice.hosts.profiles.biryani = options;

  config = with lib; mkIf cfg.enable {
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
