{ config, lib, pkgs, rice, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  inherit (rice.lib) mkMergeIf;

  cfg = config.rice.services.steam;

  options = {
    enable = mkEnableOption "Steam";
    openFirewall = {
      remotePlay = mkEnableOption "Steam remote play ports";
      sourceServer = mkEnableOption "Steam Source server ports";
    };
  };

  ricedSteam = pkgs.steam.override {
    extraPkgs = spkgs: with spkgs; [
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXScrnSaver
      libpng
      libpulseaudio
      libvorbis
      stdenv.cc.cc.lib
      libkrb5
      keyutils
      steamPackages.steamcmd
      steamPackages.steam-runtime
    ];
  };

in {
  options.rice.services.steam = options;

  config = mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        hardware.steam-hardware.enable = true;

        programs.steam = {
          enable = true;
          gamescopeSession.enable = true;
          package = ricedSteam;
        };

        environment.systemPackages = with pkgs; [
          steam-run
          protonup-qt
        ];
      };
    }

    {
      cond = cfg.enable && cfg.openFirewall.remotePlay;
      as = {
        programs.steam.remotePlay.openFirewall = true;
      };
    }

    {
      cond = cfg.enable && cfg.openFirewall.sourceServer;
      as = {
        programs.steam.dedicatedServer.openFirewall = true;
      };
    }
  ];
}
