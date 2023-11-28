### Toplevel of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, toplevel, ... }:

let
  lib = nixpkgs.lib;

  ## Return a list of all file/directory names under dir except default.nix
  allButDefault = with builtins; dir:
    (map (fn: dir + "/${fn}")
      (filter (fn: "default.nix" != fn)
        (attrNames (readDir dir))));

  ## Import all files/directories from the list returned by `allButDefault'.
  importAll = with builtins; dir: args:
    (map (fn: lib.callPackageWith args fn {}) (allButDefault dir));

  ## Librice itself
  librice = with builtins; foldl' (a: b: a // b) {
    inherit importAll allButDefault;
  } (importAll ./. {
    self = librice;
    inherit nixpkgs rice toplevel;
  });

in
librice
