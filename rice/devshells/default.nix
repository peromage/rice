{ nixpkgs, inputs, outputs, rice, ... }:

let
  librice = rice.lib;

  shells = pkgs: {
    default = pkgs.mkShell {
      packages = with pkgs.pkgsUnrestricted; [
        hello
      ];
    };
  };

in with librice; forSupportedSystems (system:
  shells (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.unrestricted-packages ];
  }))
