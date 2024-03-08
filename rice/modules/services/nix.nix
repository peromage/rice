{ config, lib, pkgs, rice, ... }:

let
  inherit (lib) mkEnableOption mkDefault;
  inherit (rice.lib) mkMergeIf;
  inherit (rice) nixpkgs;

  cfg = config.rice.services.nix;

  options = {
    enable = mkEnableOption "Nix settings";
    enableOptimization = mkEnableOption "Nix optimization" // { default = true; };
  };

in {
  options.rice.services.nix = options;

  config = mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        nixpkgs = {
          hostPlatform = mkDefault "x86_64-linux";
          config = {
            allowUnfree = true;
            allowBroken = true;
          };
          ## Make sure <nixpkgs> is consistent with the one from flake
          flake = {
            setNixPath = true;
            setFlakeRegistry = true;
          };
        };

        nix = {
          enable = true;
          channel.enable = true;

          settings = {
            sandbox = true;
            experimental-features = [ "nix-command" "flakes" ];
            trusted-users = [
              "@wheel"
            ];
            allowed-users = [
              "@wheel"
              "@users"
            ];
          };

          /* Synonyms
             pkgs.nixVersions.stable -> pkgs.nix, pkgs.nixFlakes, pkgs.nixStable
             pkgs.nixVersions.unstable -> pkgs.nixUnstable
             See: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/aliases.nix
          */
          package = pkgs.nixFlakes;
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
