{ self, nixpkgs, rice, ... }:

let
  lib = nixpkgs.lib;
  libhm = rice.flake.inputs.home-manager.lib;

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


  /* Merge multiple module block conditonally.

     To leverage lazyness and avoid infinit recursion when some module blocks
     need to be evaluated conditionally.

     Type:
       mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: with lib; mkMerge (map (x: mkIf x.cond x.as) listOfAttrs);
}
