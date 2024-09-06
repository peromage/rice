{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* A generic function to generate a module that has ability to add additonal
     modules.  Similar to the concept of override.

     `f' is a configuration generation function like `nixosSystem',
     `darwinSystem' or `homeManagerConfiguration'.

     `fGenArgs' is a function that takes a list of modules and returns an attrs that
     can be consumed by `f'.

     `modules' is a list of modules passed to `fGenArgs'.

     Type:
       mkConfiguration :: (AttrSet -> AttrSet) -> ([Path | AttrSet] -> AttrSet) -> (Path | AttrSet | [Path | AttrSet]) -> AttrSet
  */
  mkConfiguration = f: fGenArgs: modules:
    let
      virtualMake = modules:
        with lib; let savedList = toList modules;
                  in f (fGenArgs savedList)
                     // { extraModules = newModules: virtualMake (savedList ++ (toList newModules)); };
    in virtualMake modules;

  /* Merge a list of attribute sets from config top level.

     NOTE: This is a workaround to solve the infinite recursion issue when trying
     merge configs from top level.  The first level of attribute names must be
     specified explicitly.

     See: https://gist.github.com/udf/4d9301bdc02ab38439fd64fbda06ea43

     Type:
       mkMergeTopLevel :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevel = firstLevelNames: listOfAttrs:
    lib.getAttrs firstLevelNames
      (lib.mapAttrs
        (n: v: lib.mkMerge v)
        (lib.foldAttrs (n: a: [n] ++ a) [] listOfAttrs));


  /* Merge multiple module block conditonally.

     To leverage lazyness and avoid infinit recursion when some module blocks
     need to be evaluated conditionally.

     Type:
       mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: lib.mkMerge (map (x: lib.mkIf x.cond x.as) listOfAttrs);

  /* Check the `enable' attribute for each name in the set and return true if
     at least one is true;

     Type:
       anyEnable :: AttrSet -> Bool
  */
  anyEnable = attrs: lib.foldlAttrs (a: _: v: v.enable || a) false attrs;

  /* Return an attrs for names that have `enable' attribute true;

     Type:
       filterEnable :: AttrSet -> AttrSet
  */
  filterEnable = lib.filterAttrs (n: v: v.enable);
}
