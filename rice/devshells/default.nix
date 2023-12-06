{ nixpkgs, outputs, rice, ... }:

let
  librice = rice.lib;

  shells = pkgs: {
    default = pkgs.mkShell {
      packages = with pkgs.pkgsCustom; [
        hello
      ];
    };
  };

in with librice; forSupportedSystems (system:
  shells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.pkgsCustom ];
  }))
