{ config, lib, pkgs, rice, ... }:

let
  nixpkgs = rice.nixpkgs;
  cfg = config.rice.services.nix;

in with lib; {
  options.rice.services.nix = {
    enable = mkEnableOption "Nix settings";
  };

  config = mkIf cfg.enable {
    nixpkgs = {
      hostPlatform = lib.mkDefault "x86_64-linux";
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };

    nix = {
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

      /* Synonyms
         pkgs.nixVersions.stable -> pkgs.nix, pkgs.nixFlakes, pkgs.nixStable
         pkgs.nixVersions.unstable -> pkgs.nixUnstable
         See: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/aliases.nix
      */
      package = pkgs.nixFlakes;

      ## Use the nixpkgs from the toplevel flake
      registry.nixpkgs.flake = nixpkgs;
      nixPath = [
        "nixpkgs=${nixpkgs}"
        "/nix/var/nix/profiles/per-user/root/channels"
      ];
    };
  };
}
