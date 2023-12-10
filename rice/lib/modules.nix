{ self, nixpkgs, topLevel, rice, ... }:

let
  lib = nixpkgs.lib;
  libhm = rice.inputs.home-manager.lib;

in with self; {
  /* Import a NixOS top level module.

     Note that the `system' attribute is not explicitly set (default to null)
     to allow modules to set it themselves.  This allows a hermetic configuration
     that doesn't depend on the system architecture when it is imported.
     See: https://github.com/NixOS/nixpkgs/pull/177012

     Type:
       nixosTopModule :: (Path | AttrSet) -> AttrSet
  */
  nixosTopModule = topModule: lib.nixosSystem {
    specialArgs = { inherit rice; };
    modules = [ topModule ];
  };

  /* Import a HomeManager top level module.

     Note that this is a generic import so the `pkgs' needs to be passed from
     the caller.

     Type:
       homeTopModule :: AttrSet -> Path -> AttrSet
  */
  homeTopModule = pkgs: topModule: libhm.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = { inherit rice; };
    modules = [ topModule ];
  };

  /* Module root directory

     Type:
       moduleTopLevel :: String
  */
  moduleTopLevel = "${topLevel}/modules";

  /* Shorthand to get paths of NixOS modules relative to the toplevel.
     Usually used with `imports' block in a NixOS module.

     Type:
       getModules :: [String] -> [String]
  */
  getModules = prefixWith "${moduleTopLevel}/";

  /* Generate an attribute set for supported platforms.
     More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

     Type:
       forSupportedSystems :: (String -> a) -> AttrSet
  */
  forSupportedSystems = lib.genAttrs [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
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

     NOTE: Besides the cumbersome explict toplevel attribute name specifying,
     another drawback of this approach is that if certain conditions are false
     and lead to an non-existed toplevel, the evaluation could break.

     Type:
       mkMergeTopLevelCond :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevelCond = firstLevelNames: listOfConds:
    mkMergeTopLevel firstLevelNames (optionalAttrList listOfConds);
}
