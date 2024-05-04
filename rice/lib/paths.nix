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
  isFileType = name: type: type == "regular";
  isDefaultNix = name: type: name == "default.nix";

  /* Return a list of all file/directory names under dir except default.nix.

     Type:
       listDirNoDefault :: Path -> [String]
  */
  listDirNoDefault = filterDir (n: t: "default.nix" != n);

  /* Return a list of directories.

     Type:
       listDirAllDirs :: Path -> [String]
  */
  listDirAllDirs = filterDir (n: t: "directory" == t);

  /* Return a list of files.

     Type:
       listDirAllFiles :: Path -> [String]
  */
  listDirAllFiles = filterDir (n: t: "regular" == t);

  /* Return a list of files except default.nix.

     Type:
       listDirAllFilesNoDefault :: Path -> [String]
  */
  listDirAllFilesNoDefault = filterDir (n: t: "regular" == t && "default.nix" != n);
}
