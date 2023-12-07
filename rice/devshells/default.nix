{ nixpkgs, outputs, rice, ... }:

let
  librice = rice.lib;

  mkDevshells = pkgs: {
    ## Example
    default = pkgs.mkShell {
      packages = with pkgs.pkgsCustom; [
        hello
      ];
    };
  };

in with librice; forSupportedSystems (system:
  mkDevshells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.pkgsCustom ];
  }))
