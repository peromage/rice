{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vim
    wget
    curl
    rsync
    git
    git-lfs
    coreutils
  ];
}
