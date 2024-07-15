{ config, lib, pkgs, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.services.nix;

  options = with lib; {
    enable = mkEnableOption "Nix settings";
    enableOptimization = mkEnableOption "Nix optimization" // { default = true; };
  };

in {
  options.pix.services.nix = options;

  config = libpix.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        nixpkgs = {
          hostPlatform = lib.mkDefault "x86_64-linux";
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
