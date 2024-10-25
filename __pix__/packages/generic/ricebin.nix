## My personal scripts and executables.

{ pkgs, pix, ... }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "ricebin";
  version = "1.0";
  src = pix.path.bin;

  installPhase = ''
    mkdir -p $out/bin
    cp $src/* $out/bin/
  '';
}
