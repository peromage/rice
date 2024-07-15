{ config, lib, pkgs, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.services.steam;

  options = with lib; {
    enable = mkEnableOption "Steam";
    openFirewall = {
      remotePlay = mkEnableOption "Steam remote play ports";
      sourceServer = mkEnableOption "Steam Source server ports";
    };
  };

  customSteam = pkgs.steam.override {
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
      wqy_zenhei
      wqy_microhei
    ];
  };

in {
  options.pix.services.steam = options;

  config = libpix.mkMergeIf [
    {
      cond = cfg.enable;
      as = {
        hardware.steam-hardware.enable = true;

        programs.steam = {
          enable = true;
          gamescopeSession.enable = true;
          package = customSteam;
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
