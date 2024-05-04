### Top level of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, ... }:

let
  inherit (nixpkgs.lib) genAttrs mapAttrsToList filterAttrs;
  inherit (builtins) readDir;

  ## A fraction of librice
  fraction = {
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
  };

  importAllFiles = root: map
    (fn: import fn libArgs)
    (mapAttrsToList
      (n: t: root + "/${n}")
      (filterAttrs (n: t: n != "default.nix") (readDir root)));

  ## Import other lib files
  librice = let
    args = {
      self = librice;
      inherit nixpkgs rice;
    };
  in foldl' (a: b: a // b) fraction (importAllFiles ./.);

in
librice
