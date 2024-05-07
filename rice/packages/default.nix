{ nixpkgs, rice, withPkgsAllOverlays, home-manager, nix-darwin, ... }:

let
  inherit (rice.lib) joinPath mkPackageList forSupportedSystems genSpecialArgs;
  inherit (nixpkgs.lib) optionalAttrs pathExists mapAttrs;
  inherit (builtins) match;

  /* Create an AttrSet of packages from the root flake inputs
  */
  mkFlakeInputPackages = system: {
    home-manager = home-manager.packages.${system}.default;
  } // (optionalAttrs ((match ".*-darwin" system) != null) {
    nix-darwin = nix-darwin.packages.${system}.default;
  });

  ## Exclude any file or directory that has the name of the supported platform.
  ## Scanning is done once to avoid additional overhead.
  commonPackages = mkPackageList ./.;

  /* Get all packages within this directory (single file or directory) and
     platform specific packages in its directory, e.g. x86_64-linux.

     That is, for any file or directory that has the name of the supported
     platform it will be imported for that platform only.
  */
  mkPlatformPackages = system:
    let
      pkgs = withPkgsAllOverlays system;
      flakeInputPackages = mkFlakeInputPackages system;
      platformPackages = let platformPath = joinPath [./. system];
                         in optionalAttrs
                           (pathExists platformPath)
                           (import platformPath (genSpecialArgs {
                             inherit withPkgsAllOverlays;
                           }));
    in (mapAttrs
      (n: v: pkgs.unrestrictedPkgs.callPackage v rice.passthrough)
      (commonPackages // platformPackages)) // flakeInputPackages;

in forSupportedSystems mkPlatformPackages;
