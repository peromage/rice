{ nixpkgs, flake, rice, withPkgsOverlays, ... }:

let
  lib = nixpkgs.lib;
  librice = rice.lib;

  supportedSystems = librice.forSupportedSystems lib.id;

  ## Packages within this directory (single file or directory) and platform
  ## specific packages in its directory, e.g. x86_64-linux.
  mkPackages =
    let commonPackages = with librice; importListAsAttrs'
      (filterDir (n: t: !(lib.hasAttr n supportedSystems) && "default.nix" != n) ./.);
    in system:
      let
        pkgs = withPkgsOverlays system;
        platformPath = ./. + "/${system}";
        platformPackages = with lib; optionalAttrs
          (pathExists platformPath)
          (with librice; importListAsAttrs' (listDirNoDefault platformPath));
      in with lib; mapAttrs
        (n: v: pkgs.unrestrictedPkgs.callPackage v { inherit rice; })
        (commonPackages // platformPackages);

  ## Packages from inputs
  exposePackages = system: with flake.inputs; {
    home-manager = home-manager.packages.${system}.default;
  };

in with librice; forSupportedSystems (system:
  (mkPackages system) // (exposePackages system))
