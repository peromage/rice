{ self, nixpkgs, rice, ... }:

let
  inherit (nixpkgs.lib) isFunction foldl' pathExists elemAt;
  inherit (builtins) baseNameOf match;

in with self; {
  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> ((AttrSet -> a) | Path) -> a
  */
  callWithArgs = args: fn: (if isFunction fn then fn else import fn) args;

  /* Import using `rice' as the arguments.

     Type:
       callWithRice :: ((AttrSet -> a) | Path) -> a
  */
  callWithRice = callWithArgs rice;

  /* Import each module from the list with given argument.

     Type:
       callListWithArgs :: AttrSet -> [(AttrSet -> a) | Path] -> [a]
  */
  callListWithArgs = args: fns:
    let call = callWithArgs args;
    in map call fns;

  /* Treat all elements imported as attrsets and merge them into one.

     Type:
       callListAsMerged :: AttrSet -> [(AttrSet -> a) | Path] -> AttrSet
  */
  callListAsMerged = args: fns:
    let call = callWithArgs args;
    in foldl' concatAttrs {} (map call fns);

  /* Import paths from the given list.

     Type:
       importList :: [Path] -> [a]
  */
  importList = fns: map import fns;

  /* Like `importList' but instead of returning a list this returns an attrset
     with keys as the file names.

     Type:
       importListAsAttrs :: [Path] -> AttrSet
  */
  importListAsAttrs = fns: mapListToAttrs baseNameOf import fns;

  /* Similar with `importListAsAttrs' but extensions are stripped from names.

     Type:
       importListAsAttrs' :: [Path] -> AttrSet
  */
  importListAsAttrs' = fns: mapListToAttrs baseNameNoExt import fns;

  /* Append default.nix to path.

     Type:
       getDefaultFile :: (Path | String) -> String
  */
  getDefaultFile = path: path + "/default.nix";

  /* Determine if a path contains default.nix.

     Type:
       hasDefaultFile :: (Path | String) -> Bool
  */
  hasDefaultFile = path: pathExists (getDefaultFile path);

  /* Return the default.nix of path if it exists or path itself otherwise.

     Type:
       toDefaultFile :: (Path | String) -> (Path | String)
  */
  toDefaultFile = path: if hasDefaultFile path then getDefaultFile path else path;

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name:
    let
      b = baseNameOf name;
      m = match "(.*)\\.[^.]+$" b;
    in if null == m then b else elemAt m 0;
}
