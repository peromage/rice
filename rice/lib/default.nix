### Toplevel of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, toplevel, ... }:

let
  lib = nixpkgs.lib;

  /* Return a list of all file/directory names under dir except default.nix

     Type:
       allButDefault :: (Path | String) -> [String]
  */
  allButDefault = dir: with builtins; map
    (fn: dir + "/${fn}")
    (filter (fn: "default.nix" != fn) (attrNames (readDir dir)));

  ## Librice itself
  librice = let
    subset = {
      inherit allButDefault;
    };
    args = {
      self = librice;
      inherit nixpkgs rice toplevel;
    };
  in with builtins; foldl'
    (a: b: a // b)
    subset
    (map (fn: lib.callPackageWith args fn {}) (allButDefault ./.));

in
librice
