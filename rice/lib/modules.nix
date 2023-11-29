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
  getModules = prefixWith "${moduleTopLevel}/";

  /* Generate an attribute set for supported platforms.

     Type:
       forSupportedSystems :: (String -> a) -> AttrSet
  */
  forSupportedSystems = lib.genAttrs [
    "aarch64-linux"
    "x86_64-linux"
    "x86_64-darwin"
  ];

  /* Merge a list of attribute sets from config top level.
     NOTE: This is a workaround to solve the infinite recursion issue when trying
     merge configs from top level.  The first level of attribute names must be
     specified explicitly.

     See: https://gist.github.com/udf/4d9301bdc02ab38439fd64fbda06ea43

     Type:
       mkMergeTopLevel :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevel = firstLevelNames: listOfAttrs:
    with lib; getAttrs firstLevelNames
      (builtins.mapAttrs
        (n: v: mkMerge v)
        (foldAttrs (n: a: [n] ++ a) [] listOfAttrs));

  /* Like `mkMergeTopLevel' but with conditions for each subset.

     Type:
       mkMergeTopLevelCond :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevelCond = firstLevelNames: listOfConds:
    mkMergeTopLevel firstLevelNames (optionalAttrList listOfConds);
}
