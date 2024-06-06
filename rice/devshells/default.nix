{ nixpkgs, rice, withPkgsAllOverlays, ... }:

let
  inherit (rice.lib) importAllNameMapped baseNameNoExt listNonPlatformSpecific
    listPlatformSpecific forSupportedSystems;
  inherit (nixpkgs.lib) mapAttrs foldl';

  importShells = importAllNameMapped baseNameNoExt;

  commonShells = importShells (listNonPlatformSpecific ./.);

  mkDevShells = system:
    let
      pkgs = withPkgsAllOverlays system;
      callPackage = p: pkgs.unrestrictedPkgs.callPackage p rice.args;
      platformShells = listPlatformSpecific ./. system;
    in
      (mapAttrs (n: v: callPackage v) commonShells)
      // (foldl' (acc: x: acc // (callPackage x)) {} platformShells);

in forSupportedSystems mkDevShells
