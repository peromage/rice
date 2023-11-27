### Toplevel of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, toplevel, ... }:

let
  lib = nixpkgs.lib;

  importAll = with builtins;
    dir: args:
    (map (f: import (dir + "/${f}") args)
      (filter f: "default.nix" != f
        (attrNames (readDir dir))));

  librice = with builtins;
    foldl' (a: b: a // b) {} (importAll ./. {
      { self = librice; inherit nixpkgs rice toplevel; }
    }) // {
      inherit importAll;
    };

in
librice
