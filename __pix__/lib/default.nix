### Top level of the common code

{ nixpkgs, ... }@args:

let
  lib = nixpkgs.lib;
  prelude = let x = (import ./prelude.nix) (args // { self = x; }); in x;

  ## Import all nix files within this folder
  libpix = with prelude; lib.foldl'
    (acc: x: acc // x)
    {}
    (attrValues (mapImport (call (args // { self = libpix; })) ./.));

in libpix
