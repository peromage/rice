### Top level of the common code

## This lib only requires nixpkgs with an exception of other flakes from the top
## level flake inputs, e.g. home-manager.
{ nixpkgs, flakeInputs, ... }:

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

  importAllFiles = args: root: map
    (fn: import fn args)
    (mapAttrsToList
      (n: t: root + "/${n}")
      (filterAttrs (n: t: n != "default.nix") (readDir root)));

  ## Import other lib files
  librice = let
    args = flakeInputs // {
      ## Note: This nixpkgs will be used if it is different from the one from flakeInputs
      inherit nixpkgs flakeInputs;
      self = librice;
    };
  in foldl' (a: b: a // b) fraction (importAllFiles args ./.);

in
librice
