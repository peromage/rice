{ nixpkgs, rice, withPkgsOverlays, ... }:

let
  inherit (rice.lib) importListAsAttrs' filterDir isNotDefaultNix forSupportedSystems;
  inherit (nixpkgs.lib) mapAttrs;

  mkDevShells =
    let allShells = importListAsAttrs' (filterDir isNotDefaultNix ./.);
    in pkgs: mapAttrs (n: v: pkgs.callPackage v { inherit rice; }) allShells;

in forSupportedSystems (system: mkDevShells (withPkgsOverlays system))
