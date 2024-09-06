{ pix, pkgs, nixpkgs, ... }@args:

let
  lib = nixpkgs.lib;
  libpix = pix.lib;
  pathGeneric = ./generic;
  pathSystem = ./. + "/${pkgs.system}";

  /* During the import, two directories will be included:
     - generic
     - [system]

     The system directory is optional, which is something like x86_64-linux.
  */
  callPackagesIn = libpix.mapImport (fn: pkgs.callPackage fn args);

in callPackagesIn pathGeneric // lib.optionalAttrs (builtins.pathExists pathSystem) (callPackagesIn pathSystem)
