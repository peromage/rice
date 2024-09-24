{ pix, ... }:

{
  unrestrictedPkgs = final: prev: {
    unrestricted = import pix.inputs.nixpkgs {
      inherit (final) system;
      config = {
        allowUnfree = true;
        allowBroken = true;
      };
    };
  };

  pixPkgs = final: prev: {
    pix = pix.packages.${final.system};
  };
}
