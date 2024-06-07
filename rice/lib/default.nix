### Top level of the common code

## This lib only requires nixpkgs with an exception of other flakes from the top
## level flake inputs, e.g. home-manager, which should be included in specialArgs.
{ nixpkgs, specialArgs, ... }@args:

let
  lib = nixpkgs.lib;

  prelude = let x = (import ./prelude.nix) (args // { self = x; }); in x;

  ## Import all nix files within this folder
  librice = let arguments = args // specialArgs // {
    self = librice;
  };
  in with prelude; lib.foldl'
    (a: i: a // i)
    {}
    (callAllWithArgs arguments
      (listDir (n: t: isNotDefaultNix n t && isImportable n t) ./.));

in librice
