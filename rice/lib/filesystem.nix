{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) mapAttrsToList filterAttrs hasAttr elemAt;
  inherit (builtins) readDir baseNameOf match;

in with self; {
  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems id;

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       filterDir :: (String -> String -> Bool) -> Path -> [String]
  */
  filterDir = pred: dir: mapAttrsToList
    (n: t: dir + "/${n}")
    (filterAttrs pred (readDir dir));

  /* Predications used for `filterDir'. */
  isDirType = name: type: type == "directory";
  isFileType = name: type: type == "regular";
  isDefaultNix = name: type: name == "default.nix";
  isNixFile = name: type: (isFileType name type) && ((match ".+\\.nix$" name) != null);
  isBaseNameSupportedSystem = name: type: hasAttr (baseNameNoExt name) supportedSystems;
  isImportable = name: type: (isNixFile name type) || (isDirType name type);

  negateFilterDirPred = wrapReturn 2 not;

  isNotDirType = negateFilterDirPred isDirType;
  isNotFileType = negateFilterDirPred isFileType
  isNotDefaultNix = negateFilterDirPred isDefaultNix;
  isNotNixFile = negateFilterDirPred isNixFile;
  isNotBaseNameSupportSystem = negateFilterDirPred isBaseNameSupportedSystem;
  isNotImportable = negateFilterDirPred isImportable;

  /* Join a list of paths.

     Type:
       joinPaths :: [Path] -> Path
  */
  joinPaths = join "/";

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name:
    let
      b = baseNameOf name;
      m = match "(.+)\\.[^.]+$" b;
    in if null == m then b else elemAt m 0;
}
