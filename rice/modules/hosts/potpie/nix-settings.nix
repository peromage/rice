{ pkgs, rice, ... }:

let
  nixpkgs = rice.nixpkgs;
  lib = nixpkgs.lib;

in {
  ## Use unfree software
  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-linux";
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

  nix = {
    ## Enable experimental features
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };

    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };

    gc = {
      automatic = false; # Manual GC
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    ## Synonyms
    ## pkgs.nixVersions.stable -> pkgs.nix, pkgs.nixFlakes, pkgs.nixStable
    ## pkgs.nixVersions.unstable -> pkgs.nixUnstable
    ## See: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/aliases.nix
    package = pkgs.nixFlakes;

    ## Use the nixpkgs from the toplevel flake
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [ "/etc/nix/path" ];
  };

  environment.etc."nix/path/nixpkgs".source = "${nixpkgs}";
}
