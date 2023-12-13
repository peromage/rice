{ pkgs, extraPackages ? [], ... }:

let
  fhsenv = pkgs.buildFHSEnv {
    name = "Standard FHS environment";
    targetPkgs = pkgs: with pkgs; [
      gcc
      libgcc
      gnumake
      cmake
      autoconf
      libtool
    ] ++ extraPackages;
  };

in fhsenv.env
