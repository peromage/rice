{ nixpkgs, inputs, rice, withCustomPkgs, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  mkPackages =
    let allPackages = with librice; importListAsAttrs' (allButDefault ./.);
    in pkgs: with lib; mapAttrs
      (n: v: pkgs.unrestrictedPkgs.callPackage v { inherit rice; })
      allPackages;

  ## Packages from inputs
  exposePackages = system: with inputs; {
    home-manager = home-manager.packages.${system}.default;
  };

in with librice; forSupportedSystems (system:
  mkPackages (withCustomPkgs system) // (exposePackages system))
