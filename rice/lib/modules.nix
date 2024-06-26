{ self, nixpkgs, nix-darwn, home-manager, ... }:

let
  lib = nixpkgs.lib;
  libhm = home-manager.lib;
  libdw = nix-darwn.lib;

in with self; {
  /* A generice function to generate a module that has ability to add additonal
     modules.  Similar to the concept of override.

     `f' is a function like `nixosSystem' `darwinSystem' or `homeManagerConfiguration'.

     `init' is a function that takes a list of modules and returns an attrs that
     can be consumed by `f'.

     `mod' is the module itself.  It can be either a path or attrs.

     Type:
       mkTopModule :: (AttrSet -> AttrSet) -> ([Path | AttrSet] -> AttrSet) -> (Path | AttrSet) -> AttrSet
  */
  mkTopModule = f: init: mod:
    let
      add = f: init: mods:
        f (init mods) // { extraModule = mod: add f init (mods ++ [mod]); };
    in add f init [mod];

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

  /* Import a NixOS top level module.

     Note that the `system' attribute is not explicitly set (default to null)
     to allow modules to set it themselves.  This allows a hermetic configuration
     that doesn't depend on the system architecture when it is imported.
     See: https://github.com/NixOS/nixpkgs/pull/177012

     Type:
       nixosTopModule :: (Path | AttrSet) -> AttrSet
  */
  nixosTopModule = specialArgs: mkTopModule lib.nixosSystem (mods: {
    specialArgs = specialArgs;
    modules = mods;
  });

  /* Import a Darwin top level module.

     Type:
       darwinTopModule :: (Path | AttrSet) -> AttrSet
  */
  darwinTopModule = specialArgs: mkTopModule libdw.darwinSystem (mods: {
    specialArgs = specialArgs;
    modules = mods;
  });

  /* Import a HomeManager top level module.

     Note that this is a generic import so the `pkgs' needs to be passed from
     the caller.

     Type:
       homeTopModule :: AttrSet -> (Path | AttrSet) -> AttrSet
  */
  homeTopModule = pkgs: specialArgs: mkTopModule libhm.homeManagerConfiguration (mods: {
    inherit pkgs;
    extraSpecialArgs = specialArgs;
    modules = mods;
  });

}
