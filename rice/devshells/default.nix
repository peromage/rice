{ nixpkgs, rice, withPkgsOverlays, ... }:

let
  inherit (rice.lib) importListAsAttrs' listDirNoDefault forSupportedSystems;
  inherit (nixpkgs.lib) mapAttrs;

  mkDevShells =
    let allShells = importListAsAttrs' (listDirNoDefault ./.);
    in pkgs: mapAttrs (n: v: pkgs.callPackage v { inherit rice; }) allShells;

in forSupportedSystems (system: mkDevShells (withPkgsOverlays system))
