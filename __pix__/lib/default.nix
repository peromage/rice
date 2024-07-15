### Top level of the common code

## TODO: Optional arguments are not captured in args when not specified.
## See: https://nixos.wiki/wiki/Nix_Language_Quirks#Default_values_are_not_bound_in_.40_syntax
{ nixpkgs
, home-manager ? {} # Used for home config module importing.
, nix-darwin ? {} # Used for darwin module importing.
, ... }@args:

let
  lib = nixpkgs.lib;
  prelude = let x = (import ./prelude.nix) (args // { self = x; }); in x;

  ## Import all nix files within this folder
  libpix = with prelude; lib.foldl'
    (acc: x: acc // x)
    {}
    (callAll (args // { self = libpix; })
      (listDir (n: t: isNotDefaultNix n t && isImportable n t) ./.));

in libpix
