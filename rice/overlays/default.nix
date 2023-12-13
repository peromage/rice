{ nixpkgs, ... }:

{
  unrestrictedPkgs = final: prev: {
    unrestrictedPkgs = import nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };
}
