{ config, pkgs, ... }:

{
  ## Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = "fang";
    homeDirectory = "/home/fang";
    stateVersion = "23.11";
  };

  home.file = {
  };
}
