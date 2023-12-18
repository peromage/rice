{ nixpkgs, rice, withPkgsOverlays, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkDevShells =
    let allShells = with librice; importListAsAttrs' (listDirNoDefault ./.);
    in pkgs: with lib; mapAttrs (n: v: pkgs.callPackage v { inherit rice; }) allShells;

in with librice; forSupportedSystems (system: mkDevShells (withPkgsOverlays system))
