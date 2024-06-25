{ system, nixpkgs, rice, pkgsWithMyOverlays, ... }@args:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  /* Import shell derivations from files/directories.

     Each shell must be a function that returns a derivation by `pkgs.mkShell',
     `pkgs.buildFHSEnv' or any other equivalent and can be called with
     `pkgs.callPackage'.

     During the import, two directories will be included:
     - generic
     - [system]

     The system directory is optional, which is something like x86_64-linux.

     `default.nix' will be ignored.
  */

  pkgs = pkgsWithMyOverlays system;
  callAllIn = path: lib.mapAttrs
    (n: v: pkgs.callPackage v args)
    (librice.importAllNameMapped
      librice.baseNameNoExt
      (librice.listDir (n: t: librice.isNotDefaultNix n t && librice.isImportable n t) path));

  pathGeneric = ./generic;
  pathSystem = ./. + "/${system}";

in callAllIn pathGeneric // lib.optionalAttrs (builtins.pathExists pathSystem) (callAllIn (pathSystem))
