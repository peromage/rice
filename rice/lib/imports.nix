{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Like import but with predefined arguments.

     Type:
       importWithArgs :: AttrSet -> Path -> a
  */
  importWithArgs = args: path: import path args;

  /* Import with rice passed in.

     Type:
       importWithRice :: Path -> a
  */
  importWithRice = importWithArgs rice;

  /* Import all files/directories from the list returned by `allButDefault'.

     Type:
       importAll :: Path -> AttrSet -> [a]
  */
  importAll = dir: args: with builtins; map (importWithArgs args) (allButDefault dir);

  /* Treat all elements returned by importAll as attribute sets and merge them.

     Type:
       importAllMerged :: Path -> AttrSet -> AttrSet
  */
  importAllMerged = dir: args: with builtins; foldl' (a: b: a // b) {} (importAll dir args);

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
