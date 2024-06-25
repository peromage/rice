{ system, nixpkgs, rice, callPackages, pkgsWithMyOverlays, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;
  pkgs = pkgsWithMyOverlays system;
  pathGeneric = ./generic;
  pathSystem = ./. + "/${system}";

  /* Import shell derivations from files/directories.

     Each shell must be a function that returns a derivation by `pkgs.mkShell',
     `pkgs.buildFHSEnv' or any other equivalent and can be called with
     `pkgs.callPackage'.

     During the import, two directories will be included:
     - generic
     - [system]

     The system directory is optional, which is something like x86_64-linux.
  */
  callAllIn = callPackages pkgs.callPackage { inherit system; };

in callAllIn pathGeneric // lib.optionalAttrs (builtins.pathExists pathSystem) (callAllIn (pathSystem))
