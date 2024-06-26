{ system, nixpkgs, rice, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;
  pkgs = librice.pkgsWithFlakeOverlays system;
  pathGeneric = ./generic;
  pathSystem = ./. + "/${system}";

  /* During the import, two directories will be included:
     - generic
     - [system]

     The system directory is optional, which is something like x86_64-linux.
  */
  callAllIn = librice.callPackages pkgs.callPackage { inherit system; };

in callAllIn pathGeneric // lib.optionalAttrs (builtins.pathExists pathSystem) (callAllIn (pathSystem))
