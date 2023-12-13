{ pkgs, ... }:

let
  steam-with-extra = pkgs.steam.override {
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
    ];
  };

in pkgs.buildEnv {
  name = "my-steam";
  paths = with pkgs; [
    steam-with-extra
    gamescope
    steam-run
  ];
}
