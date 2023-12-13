{ nixpkgs, outputs, rice, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkDevShells =
    let allShells = with librice; importAllAsAttrs' (allButDefault ./.);
    in pkgs: with lib; mapAttrs (n: v: pkgs.callPackage v {}) allShells;

in with librice; forSupportedSystems (system:
  mkDevShells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.pkgsCustom ];
  }))
