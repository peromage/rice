{ nixpkgs, rice, withPkgsAllOverlays, ... }:

let
  inherit (rice.lib) importAllNameMapped baseNameNoExt listDir forSupportedSystems
    isSupportedSystemDir isNotSupportedSystemDir isNotDefaultNix isImportable
    mergeSetsFirstLevel;
  inherit (nixpkgs.lib) mapAttrs foldl';

  /* Import shell derivations from files/directories.
     Each shell must be a function that returns a derivation by `pkgs.mkShell',
     `pkgs.buildFHSEnv' or any other equivalent.

     Any file/directory that is not named as the supported platform, such as
     x86_64-linux, is considered to be applicable accross all supported platforms
     defined by `supportedSystems'.

     If a directory has the name that can be found in `supportedSystems', it is
     applied for that platform only.  The evaluation of the directory should be
     an attribute set containing shell definitions just like any others,

     Having said that, the importing is divided into two parts: generic and
     platform specific.  Equivalent as the follow:

     {
       x86_64-linux = { foo = <lambda>; bar = <lambda>; };
       aarch64-linux = { foo = <lambda>; bar = <lambda>; };
     }

     and

     {
       x86_64-linux = { baz = <lambda>; };
       aarch64-linux = { qux = <lambda>; };
     }

     These two sets will be merged at the first level before invoked by
     `callPackage'.

     {
       x86_64-linux = { foo = <lambda>; bar = <lambda>; baz = <lambda>; };
       aarch64-linux = { foo = <lambda>; bar = <lambda>; qux = <lambda>; };
     }
  */
  platformShells =
    let
      importPackage = importAllNameMapped baseNameNoExt;
      generic = importPackage (listDir
        (n: v:
          isNotSupportedSystemDir n v
          && isImportable n v
          && isNotDefaultNix n v)
        ./.);
      platforms = importPackage (listDir isSupportedSystemDir ./.);
    in mergeSetsFirstLevel [(forSupportedSystems (system: generic))  platforms];


  mkDevShells = system: set:
    let
      pkgs = withPkgsAllOverlays system;
      callPackage = p: pkgs.callPackage p rice.args;
    in mapAttrs (n: v: callPackage v) set;

in mapAttrs mkDevShells platformShells
