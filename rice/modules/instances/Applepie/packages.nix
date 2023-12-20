{ pkgs, ... }:

{
  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    tmux
    git
    git-lfs
    rsync
    curl
    wget
    tree
  ];
}
