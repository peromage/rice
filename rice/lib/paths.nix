{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) mapAttrsToList filterAttrs;
  inherit (builtins) readDir;

in with self; {
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
  isNotDirType = name: type: ! isDirType name type;
  isFileType = name: type: type == "regular";
  isNotFileType = name: type: ! isFileType name type;
  isDefaultNix = name: type: name == "default.nix";
  isNotDefaultNix = name: type: ! isDefaultNix name type;
}
