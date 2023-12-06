{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;
  libhm = rice.inputs.home-manager.lib;

in with self; {
  /* Import a NixOS toplevel module.

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

  /* Import a HomeManager top module.
     TODO: Maybe there is a better way to configure pkgs architecture modularly?

     Type:
       homemanagerTopModule :: [(AttrSet -> AttrSet -> AttrSet)] -> (Path | AttrSet) -> AttrSet
  */
  homemanagerTopModule = overlays: topModule: forSupportedSystems (system:
    libhm.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        inherit overlays;
      };
      extraSpecialArgs = { inherit rice; };
      modules = [ topModule ];
    });

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

     Type:
       mkMergeTopLevelCond :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevelCond = firstLevelNames: listOfConds:
    mkMergeTopLevel firstLevelNames (optionalAttrList listOfConds);
}
