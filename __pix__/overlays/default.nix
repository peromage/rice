{ pix, ... }:

{
  unrestrictedPkgs = final: prev: {
    unrestrictedPkgs = import pix.inputs.nixpkgs {
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
