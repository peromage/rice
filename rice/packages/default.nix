{ nixpkgs, inputs, rice, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkPackages =
    let allPackages = with librice; importAllAsAttrs' (allButDefault ./.);
    in pkgs: with lib; mapAttrs (n: v: pkgs.callPackage v { inherit rice; }) allPackages;

  ## Packages from inputs
  exposePackages = system: with inputs; {
    home-manager = home-manager.packages.${system}.default;
  };

in with librice; forSupportedSystems (system:
  mkPackages (import nixpkgs {
    inherit system;
    overlays = [ outputs.overlays.pkgsCustom ];
  }) // (exposePackages system))
