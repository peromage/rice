{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  ## Like import but with predefined arguments
  importWithArgs = args: path: lib.callPackageWith args path {};

  ## Import with rice passed in
  importWithRice = importWithArgs rice;

  ## Import all files/directories from the list returned by `allButDefault'.
  ## The result is a list.
  importAll = dir: args: with builtins; map (importWithArgs args) (allButDefault dir);

  ## Treat all elements returned by importAll as attribute sets and merge them.
  ## The result is an attribute set.
  importAllMerged = dir: args: with builtins; foldl' (a: b: a // b) {} (importAll dir args);

  ## Append default.nix to path
  getDefaultFile = path: path + "/default.nix";

  ## Determine if a path contains default.nix
  hasDefaultFile = path: builtins.pathExists (getDefaultFile path);

  ## Return the default.nix of path if it exists or path itself otherwise
  toDefaultFile = path: if hasDefaultFile path then getDefaultFile path else path;

  ## Prepend a prefix to a list of strings
  withPrefix = prefix: list: map (i: prefix + i) list;
}
