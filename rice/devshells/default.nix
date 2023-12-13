{ nixpkgs, outputs, rice, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkDevshells =
    let allPackages = with librice; importAllAsAttrs (allButDefault ./.);
    in pkgs: with lib; mapAttrs'
      ## Strip extensions from names
      (n: v: nameValuePair (librice.baseNameNoExt n) (pkgs.callPackage v {}))
      allPackages;

in with librice; forSupportedSystems (system:
  mkDevshells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.pkgsCustom ];
  }))
