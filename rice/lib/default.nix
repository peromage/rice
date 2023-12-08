### Top level of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, topLevel, ... }:

let
  lib = nixpkgs.lib;

  ## Auxiliary file functions
  debris = rec {
    /* A generic function that filters all the files/directories under the given
       directory.  Return a list of names prepended with the given directory.

       Type:
       allWithFilter :: (String -> a -> Bool) -> Path -> [String]
    */
    allWithFilter = f: dir: with lib; mapAttrsToList
      (n: v: dir + "/${n}")
      (filterAttrs f (builtins.readDir dir));

    /* Return a list of all file/directory names under dir except default.nix

     Type:
       allButDefault :: Path -> [String]
    */
    allButDefault = allWithFilter (n: v: "default.nix" != n);

    /* Return a list of directories.

       Type:
         allDirs :: Path -> [String]
    */
    allDirs = allWithFilter (n: v: "directory" == v);

    /* Return a list of files.

       Type:
         allFiles :: Path -> [String]
    */
    allFiles = allWithFilter (n: v: "regular" == v);
  };

  ## Librice itself
  librice = let
    args = {
      self = librice;
      inherit nixpkgs rice topLevel;
    };
  in with builtins; foldl'
    (a: b: a // b)
    debris
    (map (fn: import fn args) (debris.allButDefault ./.));

in
librice
