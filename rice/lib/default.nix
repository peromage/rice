### Top level of the common code

## This lib only requires nixpkgs with an exception of other flakes from the top
## level flake inputs, e.g. home-manager, which should be included in specialArgs.
{ nixpkgs, specialArgs, ... }@args:

let
  lib = nixpkgs.lib;

  prelude = let
    f = import ./prelude.nix;
    x = f { inherit nixpkgs; self = x; };
  in x;

  call = args: node: with prelude; lib.foldl'
    (a: i: a // i)
    {}
    (callAllWithArgs args
      (listDir (n: t: isNotDefaultNix n t && isImportable n t) node));

  ## Import all nix files within this folder
  librice = let
    arguments = args // specialArgs // {
      self = librice;
    };
  in call arguments ./.;

in librice
