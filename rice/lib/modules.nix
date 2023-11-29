{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Import a NixOS toplevel module.

     Type:
       buildNixOS :: String -> (Path | String) -> AttrSet
  */
  buildNixOS = system: topModule: lib.nixosSystem {
    inherit system;
    specialArgs = { inherit rice system; };
    modules = [ topModule ];
  };

  /* Used to import an instance in a toplevel flake.
     Alias of `importWithRice'.
     Type:
       importWithRice :: ((AttrSet -> a) | Path) -> a

  */
  importNixOS = importWithRice;

  /* Module root directory

     Type:
       moduleTopLevel :: String
  */
  moduleTopLevel = toplevel + "/modules";

  /* Shorthand to get paths of NixOS modules relative to the toplevel.
     Usually used with `imports' block in a NixOS module.

     Type:
       getModules :: [String] -> [String]
  */
  getModules = withPrefix "${moduleTopLevel}/";

  /* Generate an attribute set for supported platforms.

     Type:
       forSupportedSystems :: (String -> a) -> AttrSet
  */
  forSupportedSystems = lib.genAttrs [
    "aarch64-linux"
    "x86_64-linux"
    "x86_64-darwin"
  ];
}
