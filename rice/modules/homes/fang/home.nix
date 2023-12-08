{ rice, ... }:

{
  imports = rice.lib.allWithFilter
    (n: v:
      "regular" == v
      && "default.nix" != n
      && "home.nix" != n)
    ./.;

  ## Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    username = "fang";
    homeDirectory = "/home/fang";
    stateVersion = "23.11";
  };
}
