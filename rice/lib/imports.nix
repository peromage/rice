{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction;
  inherit (builtins) baseNameOf;

in with self; {
  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> ((AttrSet -> a) | Path) -> a
  */
  callWithArgs = args: fn: (if isFunction fn then fn else import fn) args;

  /* Import each module from the list with given argument.

     Type:
       callListWithArgs :: AttrSet -> [(AttrSet -> a) | Path] -> [a]
  */
  callListWithArgs = args: map (callWithArgs args);

  /* Import paths from the given list.

     Type:
       importList :: [Path] -> [a]
  */
  importList = map import;

  /* Like `importList' but instead of returning a list this returns an attrset
     with keys as the file names.

     Type:
       importListAsAttrs :: [Path] -> AttrSet
  */
  importListAsAttrs = mapListToAttrs baseNameOf import;

  /* Similar with `importListAsAttrs' but extensions are stripped from names.

     Type:
       importListAsAttrs' :: [Path] -> AttrSet
  */
  importListAsAttrs' = mapListToAttrs baseNameNoExt import;
}
