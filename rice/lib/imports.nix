{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;

in {
  ## Like import but with predefined arguments
  importWithArgs = args: path: import path args;

  ## Import with rice passed in
  importWithRice = path: lib.callPackageWith rice path {};

  ## Append default.nix to path
  getDefaultFile = path: path + "/default.nix";

  ## Determine if a path contains default.nix
  hasDefaultFile = path: builtins.pathExists (self.getDefaultFile path);

  ## Return the default.nix of path if it exists or path itself otherwise
  toDefaultFile = path: if self.hasDefaultFile path then self.getDefaultFile path else path;

  ## Prepend a prefix to a list of strings
  withPrefix = prefix: list: map (i: prefix + i) list;
}
