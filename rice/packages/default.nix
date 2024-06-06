{ nixpkgs, rice, withPkgsAllOverlays, home-manager, nix-darwin, ... }:

let
  inherit (rice.lib) listNonPlatformSpecific listPlatformSpecific baseNameNoExt
    importAllNameMapped forSupportedSystems;
  inherit (nixpkgs.lib) mapAttrs optionalAttrs;
  inherit (builtins) match;

  /* Create an AttrSet of packages from the root flake inputs
  */
  mkFlakeInputPackages = system: {
    home-manager = home-manager.packages.${system}.default;
  } // optionalAttrs ((match ".*-darwin" system) != null) {
    nix-darwin = nix-darwin.packages.${system}.default;
  };

  importAll = importAllNameMapped baseNameNoExt;

  ## Exclude any file or directory that has the name of the supported platform.
  ## Scanning is done once to avoid additional overhead.
  commonPackages = importAll (listNonPlatformSpecific ./.);

  /* Get all packages within this directory (single file or directory) and
     platform specific packages in its directory, e.g. x86_64-linux.

     That is, for any file or directory that has the name of the supported
     platform it will be imported for that platform only.
  */
  mkPlatformPackages = system:
    let
      pkgs = withPkgsAllOverlays system;
      flakeInputPackages = mkFlakeInputPackages system;
      platformPackages = importAll (listPlatformSpecific ./. system);
    in
      (mapAttrs
        (n: v: pkgs.unrestrictedPkgs.callPackage v rice.args)
        (commonPackages // platformPackages))
      // flakeInputPackages;

in forSupportedSystems mkPlatformPackages
