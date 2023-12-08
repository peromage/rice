{ config, pkgs, ... }:

{
  ## Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.username = "fang";
  home.homeDirectory = "/home/fang";
  home.stateVersion = "23.11";
  home.packages = [
  ];

  home.file = {
  };

  ## Alternatively source in a manual way
  ##  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  ## or
  ##  /etc/profiles/per-user/fang/etc/profile.d/hm-session-vars.sh
  home.sessionVariables = {
    # EDITOR = "emacs";
  };
}
