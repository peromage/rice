### Top level of the common code

{ nixpkgs
, specialArgs ? {} # Passed around when invoking module importing.
, home-manager ? {} # Used for home config module importing.
, nix-darwin ? {} # Used for darwin module importing.
, ... } @ args:

let
  lib = nixpkgs.lib;

  prelude = let x = (import ./prelude.nix) (args // { self = x; }); in x;

  ## Import all nix files within this folder
  librice = let
    arguments = args // {
      ## TODO: Optional arguments are captured in args when not specified.
      ## See: https://nixos.wiki/wiki/Nix_Language_Quirks
      inherit specialArgs home-manager nix-darwin;
      self = librice;
    };
    ## Initial content as a part of librice
    init = {
      inherit arguments;
    };
  in with prelude; lib.foldl'
    (a: i: a // i)
    init
    (callAllWithArgs arguments
      (listDir (n: t: isNotDefaultNix n t && isImportable n t) ./.));

in librice
