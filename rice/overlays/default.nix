{ nixpkgs, ... }:

{
  pkgsCustom = final: prev: {
    pkgsCustom = import nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };
}
