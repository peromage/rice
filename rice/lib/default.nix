### Top level of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, ... }:

let
  lib = nixpkgs.lib;

  ## Auxiliary file functions
  debris = rec {
    /* Generate an attribute set for supported platforms.
       More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

       Type:
         forSupportedSystems :: (String -> a) -> AttrSet
    */
    forSupportedSystems = lib.genAttrs [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    /* A generic function that filters all the files/directories under the given
       directory.  Return a list of names prepended with the given directory.

       Type:
       allWithFilter :: (String -> String -> Bool) -> Path -> [String]
    */
    allWithFilter = f: dir: with lib; mapAttrsToList
      (n: t: dir + "/${n}")
      (filterAttrs f (builtins.readDir dir));

    /* Return a list of all file/directory names under dir except default.nix.

     Type:
       allButDefault :: Path -> [String]
    */
    allButDefault = allWithFilter (n: t: "default.nix" != n);

    /* Return a list of directories.

       Type:
         allDirs :: Path -> [String]
    */
    allDirs = allWithFilter (n: t: "directory" == t);

    /* Return a list of files.

       Type:
         allFiles :: Path -> [String]
    */
    allFiles = allWithFilter (n: t: "regular" == t);

    /* Return a list of files except default.nix.

       Type:
         allFiles :: Path -> [String]
    */
    allFilesButDefault = allWithFilter (n: t: "regular" == t && "default.nix" != n);
  };

  ## Librice itself
  librice = let
    args = {
      self = librice;
      inherit nixpkgs rice;
    };
  in with builtins; foldl'
    (a: b: a // b)
    debris
    (map (fn: import fn args) (debris.allButDefault ./.));

in
librice
