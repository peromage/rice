{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ## Most used CLI
    vim
    wget
    curl
    rsync
    git
    git-lfs
    coreutils

    ## File system
    ntfs3g
    exfat
    exfatprogs
    e2fsprogs

    ## Disk
    parted

    ## Apps
    appimage-run
  ];
}
