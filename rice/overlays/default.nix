{ inputs, rice, ... }:

let
  librice = rice.lib;

in {
  unrestricted-packages = final: prev: {
    pkgsUnrestricted = import inputs.nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };
}
