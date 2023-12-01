{ nixpkgs, inputs, rice, ... }:

let
  librice = rice.lib;

  customPackages = pkgs: {
    ## TODO: Add more packages
  };

  exposedPackages = system: with inputs; {
    home-manager = home-manager.packages.${system}.default;
  };

in with librice; forSupportedSystems (system:
  (customPackages nixpkgs.legacyPackages.${system})
  // (exposedPackages system))
