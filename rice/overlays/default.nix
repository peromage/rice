{ nixpkgs, ... }:

{
  pkgsUnrestricted = final: prev: {
    pkgsUnrestricted = import nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };
}
