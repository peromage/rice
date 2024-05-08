{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction genAttrs;
  inherit (builtins) baseNameOf;

in with self; {
  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> ((AttrSet -> Any) | Path) -> Any
  */
  callWithArgs = args: fn: (if isFunction fn then fn else import fn) args;

  /* Import each module from the list with given argument.

     Type:
       callListWithArgs :: AttrSet -> [(AttrSet -> a) | Path] -> [Any]
  */
  callListWithArgs = args: map (callWithArgs args);

  /* Import paths from the given list.

     Type:
       importList :: [Path] -> [Any]
  */
  importList = map import;

  /* Like `importList' but instead of returning a list this returns an attrset
     with file names as the attributes.

     Type:
       importListAsAttrs :: [Path] -> AttrSet
  */
  importListAsAttrs = list: genAttrs list import;
}
