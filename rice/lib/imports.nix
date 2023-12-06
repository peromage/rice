{ self, nixpkgs, toplevel, rice, ... }:

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
       callAsMerged :: AttrSet -> [Path] -> AttrSet
  */
  callAsMerged = args: listOfPaths: builtins.foldl'
    concatAttr {} (map (callWithArgs args) listOfPaths);

  /* Import all files/directories under the given path excluding `default.nix'.

     Type:
       importAllButDefault :: Path -> [a]
  */
  importAllButDefault = dir: map import (allButDefault dir);

  /* Import all directories under the given path.

     Type:
       importAllDirs :: Path -> [a]
  */
  importAllDirs = dir: map import (allDirs dir);

  /* Import all files under the given path.

     Type:
       importAllFiles :: Path -> [a]
  */
  importAllFiles = dir: map import (allFiles dir);

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
}
