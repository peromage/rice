{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Import the given path with predefined arguments.

     Type:
       call :: AttrSet -> ((AttrSet -> Any) | Path) -> Any
  */
  call = args: fn: (if lib.isFunction fn then fn else import fn) args;

  /* Import each module from the list with given argument.

     Type:
       callAll :: AttrSet -> [(AttrSet -> Any) | Path] -> [Any]
  */
  callAll = args: map (call args);

  /* Import all modules under the top level directory.
     The returned value is an attribute set with all files/directories as names
     and contents as values.  All names have extensions stripped.

     Type:
       importAll :: Path -> AttrSet
  */
  importAll = top: with lib; mapAttrs'
    (n: v: nameValuePair (baseNameNoExt n) v)
    (genAttrs
      (listDir (n: t: isNotDisabled n t && isNotDefaultNix n t && isImportable n t) top)
      import);

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: lib.mapAttrsToList
    (n: t: builtins.toString (dir + "/${n}"))
    (lib.filterAttrs pred (builtins.readDir dir));

  /* Predications used for `listDir'. */
  isDirType = name: type: type == "directory";
  isFileType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: name == "default.nix";
  isNixFile = name: type: isNotDirType name type && builtins.match ".+\\.nix$" name != null;
  isImportable = name: type: isDirType name type || isNixFile name type;
  isDisabled = name: type: builtins.match "^DISABLED-.*" name != null;

  isNotDirType = name: type: ! isDirType name type;
  isNotFileType = name: type: ! isFileType name type;
  isNotSymbolicType = name: type: ! isSymbolicType name type;
  isNotDefaultNix = name: type: ! isDefaultNix name type;
  isNotNixFile = name: type: ! isNixFile name type;
  isNotImportable = name: type: ! isImportable name type;
  isNotDisabled = name: type: ! isDisabled name type;

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name:
    let
      b = builtins.baseNameOf name;
      m = builtins.match "(.+)(\\.[^.]+)$" b;
    in if null == m then b else lib.elemAt m 0;
}
