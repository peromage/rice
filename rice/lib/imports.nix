{ self, nixpkgs, toplevel, rice, ... }:

{
  ## Like import but with predefined arguments
  importWithArgs = args: path: import path args;

  ## Similar with importWithArgs but with rice added
  importWithRice = args: path: import path (args // { inherit rice; });

  ## Append default.nix to path
  getDefaultFile = path: path + "/default.nix";

  ## Determine if a path contains default.nix
  hasDefaultFile = path: builtins.pathExists (self.getDefaultFile path);

  ## Return the default.nix of path if it exists or path itself otherwise
  toDefaultFile = path: if self.hasDefaultFile path then self.getDefaultFile path else path;

  ## Prepend a prefix to a list of strings
  withPrefix = prefix: list: map (i: prefix + i) list;
}
