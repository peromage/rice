{ nixpkgs, rice, ... }:

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

  ricePkgs = final: prev: {
    ricePkgs = rice.packages.${final.system};
  };
}
