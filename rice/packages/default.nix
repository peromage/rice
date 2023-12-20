{ nixpkgs, flake, rice, withPkgsOverlays, ... }:

let
  inherit (rice.lib) forSupportedSystems importListAsAttrs' listDirNoDefault filterDir;
  inherit (nixpkgs.lib) hasAttr optionalAttrs pathExists mapAttrs id;
  inherit (builtins) match;

  supportedSystems = forSupportedSystems id;

  ## Packages within this directory (single file or directory) and platform
  ## specific packages in its directory, e.g. x86_64-linux.
  mkPackages =
    let commonPackages = importListAsAttrs'
      (filterDir (n: t: !(hasAttr n supportedSystems) && "default.nix" != n) ./.);
    in system:
      let
        pkgs = withPkgsOverlays system;
        platformPath = ./. + "/${system}";
        platformPackages = optionalAttrs
          (pathExists platformPath)
          (importListAsAttrs' (listDirNoDefault platformPath));
      in mapAttrs
        (n: v: pkgs.unrestrictedPkgs.callPackage v { inherit rice; })
        (commonPackages // platformPackages);

  ## Packages from inputs
  exposePackages = system: with flake.inputs; {
    home-manager = home-manager.packages.${system}.default;
  } // (optionalAttrs (null != (match ".*-darwin" system)) {
    nix-darwin = nix-darwin.packages.${system}.default;
  });

in forSupportedSystems (system:
  (mkPackages system) // (exposePackages system))
