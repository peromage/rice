{ nixpkgs, outputs, rice, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkDevShells =
    let allShells = with librice; importListAsAttrs' (allButDefault ./.);
    in pkgs: with lib; mapAttrs (n: v: pkgs.callPackage v { inherit rice; }) allShells;

in with librice; forSupportedSystems (system:
  mkDevShells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.unrestrictedPkgs ];
  }))
