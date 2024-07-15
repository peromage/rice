{ nixpkgs, pix, ... }:

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

  pixPkgs = final: prev: {
    pixPkgs = pix.packages.${final.system};
  };
}
