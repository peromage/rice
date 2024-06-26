{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Generate an attribute set for supported platforms.
       More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

     Type:
       forSupportedSystems :: (String -> Any) -> AttrSet
  */
  forSupportedSystems = lib.genAttrs [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
  ];

  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems lib.id;

  /* Check if the given system is in the supported list.

     Type:
       isSupportedSystem :: String -> Bool
  */
  isSupportedSystem = system: builtins.hasAttr system supportedSystems;

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

  /* Returns an attrset with file names as the attributes and imported content
     as the values.

     Type:
       importAll :: [String] -> AttrSet
  */
  importAll = list: lib.genAttrs list import;

  /* Similar with `importAll' but transforms the attribute names with the given
     function.
     Note that if there are duplicated results of attribute names, only the first
     one takes effect.

     Type:
       importAllNameMapped :: (String -> String) -> [String] -> AttrSet
  */
  importAllNameMapped = func: list: with lib; mapAttrs'
    (n: v: nameValuePair (func n) v)
    (importAll list);

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
  isSupportedSystemDir = name: type: isSupportedSystem name && isDirType name type;
  isDisabled = name: type: builtins.match "^DISABLED-.*" name != null;

  isNotDirType = name: type: ! isDirType name type;
  isNotFileType = name: type: ! isFileType name type;
  isNotSymbolicType = name: type: ! isSymbolicType name type;
  isNotDefaultNix = name: type: ! isDefaultNix name type;
  isNotNixFile = name: type: ! isNixFile name type;
  isNotImportable = name: type: ! isImportable name type;
  isNotSupportedSystemDir = name: type: ! isSupportedSystemDir name type;
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
