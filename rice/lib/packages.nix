{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) hasAttr id;

in with self; {
  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems id;

  /* Create an AttrSet of packages from the given directory.
     Each attribute name is the file base name without extension.

     Note that the exceptions are, a) if the file/directory name is defined
     in the `supportedSystems'; b) if it is `default.nix'.
     For those files/directories they will not be imported by this function.

     Type:
       mkPackageList :: Path -> AttrSet
  */
  mkPackageList = node: importListAsAttrs' (filterDir (n: t: (isNotDefaultNix n) && !(hasAttr n supportedSystems)) node);
}
