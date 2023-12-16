{ config, lib, pkgs, rice, ... }:

let
  librice = rice.lib;
  nixpkgs = rice.nixpkgs;
  cfg = config.rice.services.nix;

in with lib; {
  options.rice.services.nix = {
    enable = mkEnableOption "Nix settings";
    enableOptimization = mkEnableOption "Nix optimization" // { default = true; };
  };

  config = librice.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        nixpkgs = {
          hostPlatform = lib.mkDefault "x86_64-linux";
          config = {
            allowUnfree = true;
            allowBroken = true;
          };
        };

        nix = {
          settings.experimental-features = [ "nix-command" "flakes" ];

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

    {
      cond = cfg.enable && cfg.enableOptimization;
      as = {
        nix = {
          settings.auto-optimise-store = true;

          optimise = {
            automatic = true;
            dates = [ "weekly" ];
          };

          gc = {
            automatic = true;
            persistent = true;
            dates = "weekly";
            options = "--delete-older-than 30d";
            randomizedDelaySec = "0";
          };
        };
      };
    }
  ];
}
