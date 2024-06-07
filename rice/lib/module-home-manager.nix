{ self, home-manager, ... }:

let libhm = home-manager.lib;
in with self; {
  /* Import a HomeManager top level module.

     Note that this is a generic import so the `pkgs' needs to be passed from
     the caller.

     Type:
       homeTopModule :: AttrSet -> (Path | AttrSet) -> AttrSet
  */
  homeTopModule = pkgs: mkTopModule libhm.homeManagerConfiguration (mods: {
    inherit pkgs;
    extraSpecialArgs = genSpecialArgs {};
    modules = mods;
  });
}
