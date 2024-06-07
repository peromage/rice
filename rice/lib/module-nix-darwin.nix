{ self, nix-darwin, ... }:

let libdw = nix-darwin.lib;
in with self; {
  /* Import a Darwin top level module.

     Type:
       darwinTopModule :: (Path | AttrSet) -> AttrSet
  */
  darwinTopModule = mkTopModule libdw.darwinSystem (mods: {
    specialArgs = genSpecialArgs {};
    modules = mods;
  });
}
