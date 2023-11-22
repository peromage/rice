### Toplevel of the common code

## The rice parameter should only be used for passing down to the OS modules.
## The library itself should not know any implementation details of it so that
## functionalities provided by this library is guaranteed to be generic.
{ nixpkgs, rice, toplevel, ... }:

let
  importSubset = path: lib.callPackageWith
    { self = librice; inherit nixpkgs rice toplevel; }
    (import path)
    {};
  lib = nixpkgs.lib;
  librice = (importSubset ./imports.nix)
            // (importSubset ./os.nix);

in
librice
