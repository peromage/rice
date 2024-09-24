{ config, lib, pkgs, pix, ... }:

let
  inherit (pix.inputs) nixpkgs;
  cfg = config.pix.services.nix;

in with lib; {
  options.pix.services.nix = {
    enable = mkEnableOption "Nix settings";
    enableOptimization = mkEnableOption "Nix optimization" // { default = true; };
  };

  config = mkMerge [
    (mkIf cfg.enable {
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

        nixPath = [ "nixpkgs=${nixpkgs}" ];
      };
    })

    (mkIf (cfg.enable && cfg.enableOptimization) {
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
    })
  ];
}
