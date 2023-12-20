{ mkProfileOptions, ... }:
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;

  cfg = config.rice.hosts.profiles.biryani;

  options = mkProfileOptions {
    name = "biryani";
  };

in {
  options.rice.hosts.profiles.biryani = options;

  config = mkIf cfg.enable {
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
