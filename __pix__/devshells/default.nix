{ system, nixpkgs, pix, ... }:

let
  lib = nixpkgs.lib;
  libpix = pix.lib;
  pkgs = libpix.flakeOverlaidPkgs system;
  pathGeneric = ./generic;
  pathSystem = ./. + "/${system}";

  /* During the import, two directories will be included:
     - generic
     - [system]

     The system directory is optional, which is something like x86_64-linux.
  */
  callAllIn = libpix.flakeCallPackages pkgs.callPackage { inherit system; };

in callAllIn pathGeneric // lib.optionalAttrs (builtins.pathExists pathSystem) (callAllIn (pathSystem))
