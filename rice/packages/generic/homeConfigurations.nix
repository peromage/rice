{ system, nixpkgs, homeConfigurations, ... }:

nixpkgs.lib.mapAttrs
  (n: v: v // { type = "derivation"; name = "homeConfigurations"; })
  homeConfigurations.${system}
