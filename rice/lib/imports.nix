{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction hasAttr id;
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

  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems id;

  /* Create an AttrSet of Nix expressions from the given directory.
     Each attribute name is the file base name without extension.

     Note that the exceptions are, a) if the file/directory name is defined
     in the `supportedSystems'; b) if it is `default.nix'.
     For those files/directories they will not be imported by this function.

     Type:
       mkPackageList :: Path -> AttrSet
  */
  importNonPlatformSpecific = node:
    importListAsAttrs' (filterDir
      (n: t: (isNotDefaultNix n t)
             && !(hasAttr n supportedSystems)
             && ((isNixFile n t) || (isDirType n t)))
      node);
}
