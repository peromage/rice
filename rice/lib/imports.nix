{ self, nixpkgs, rice, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> Path -> a
  */
  callWithArgs = args: path: import path args;

  /* Import with rice passed in.

     Type:
       callWithRice :: Path -> a
  */
  callWithRice = callWithArgs rice;

  /* Treat all elements imported as attrsets and merge them into one.

     Type:
       callListAsMerged :: AttrSet -> [Path] -> AttrSet
  */
  callListAsMerged = args: listOfPaths: builtins.foldl'
    concatAttrs {} (map (callWithArgs args) listOfPaths);

  /* Import paths from the given list.

     Type:
       importList :: [Path] -> [a]
  */
  importList = listOfPaths: map import listOfPaths;

  /* Like `importList' but instead of returning a list this returns an attrset
     with keys as the file names.

     Type:
       importListAsAttrs :: [Path] -> AttrSet
  */
  importListAsAttrs = listOfPaths: with lib;
    mapListToAttrs baseNameOf import listOfPaths;

  /* Similar with `importListAsAttrs' but extensions are stripped from names.

     Type:
       importListAsAttrs' :: [Path] -> AttrSet
  */
  importListAsAttrs' = listOfPaths: with lib;
    mapListToAttrs baseNameNoExt import listOfPaths;

  /* Append default.nix to path.

     Type:
       getDefaultFile :: (Path | String) -> String
  */
  getDefaultFile = path: path + "/default.nix";

  /* Determine if a path contains default.nix.

     Type:
       hasDefaultFile :: (Path | String) -> Bool
  */
  hasDefaultFile = path: builtins.pathExists (getDefaultFile path);

  /* Return the default.nix of path if it exists or path itself otherwise.

     Type:
       toDefaultFile :: (Path | String) -> (Path | String)
  */
  toDefaultFile = path: if hasDefaultFile path then getDefaultFile path else path;

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name: with builtins;
    let
      b = baseNameOf name;
      m = match "(.*)\\.[^.]+$" b;
    in if null == m then b else elemAt m 0;
}
