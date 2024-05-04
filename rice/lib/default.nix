### Top level of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, ... }:

let
  inherit (nixpkgs.lib) genAttrs mapAttrsToList foldl' filterAttrs;
  inherit (builtins) readDir;

  ## Auxiliary file functions
  debris = rec {
    /* Generate an attribute set for supported platforms.
       More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

       Type:
         forSupportedSystems :: (String -> a) -> AttrSet
    */
    forSupportedSystems = genAttrs [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    /* A generic function that filters all the files/directories under the given
       directory.  Return a list of names prepended with the given directory.

       Type:
         filterDir :: (String -> String -> Bool) -> Path -> [String]
    */
    filterDir = pred: dir: mapAttrsToList
      (n: t: dir + "/${n}")
      (filterAttrs pred (readDir dir));

    /* Return a list of all file/directory names under dir except default.nix.

       Type:
         listDirNoDefault :: Path -> [String]
    */
    listDirNoDefault = filterDir (n: t: "default.nix" != n);

    /* Return a list of directories.

       Type:
         listDirAllDirs :: Path -> [String]
    */
    listDirAllDirs = filterDir (n: t: "directory" == t);

    /* Return a list of files.

       Type:
         listDirAllFiles :: Path -> [String]
    */
    listDirAllFiles = filterDir (n: t: "regular" == t);

    /* Return a list of files except default.nix.

       Type:
         listDirAllFilesNoDefault :: Path -> [String]
    */
    listDirAllFilesNoDefault = filterDir (n: t: "regular" == t && "default.nix" != n);
  };

  ## Librice itself
  librice = let
    args = {
      self = librice;
      inherit nixpkgs rice;
    };
  in foldl'
    (a: b: a // b)
    debris
    (map (fn: import fn args) (debris.listDirNoDefault ./.));

in
librice
