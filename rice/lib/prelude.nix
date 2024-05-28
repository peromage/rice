{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction genAttrs mapAttrsToList filterAttrs elemAt
    pathExists optional mapAttrs' nameValuePair id;
  inherit (builtins) readDir baseNameOf match hasAttr toString;

in with self; {
  /* Generate an attribute set for supported platforms.
       More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

     Type:
       forSupportedSystems :: (String -> Any) -> AttrSet
  */
  forSupportedSystems = genAttrs [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
  ];

  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems id;

  /* Check if the given system is in the supported list.

     Type:
       isSupportedSystem :: String -> Bool
  */
  isSupportedSystem = system: hasAttr system supportedSystems;


  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> ((AttrSet -> Any) | Path) -> Any
  */
  callWithArgs = args: fn: (if isFunction fn then fn else import fn) args;

  /* Import each module from the list with given argument.

     Type:
       callAllWithArgs :: AttrSet -> [(AttrSet -> Any) | Path] -> [Any]
  */
  callAllWithArgs = args: map (callWithArgs args);

  /* Returns an attrset with file names as the attributes and imported content
     as the values.

     Type:
       importAll :: [Path] -> AttrSet
  */
  importAll = list: genAttrs list import;

  /* Similar with `importAll' but normalize the attribute names with the given
     function.
     Note that if there are duplicated results of attribute names, only the first
     one takes effect.

     Type:
       importAllNormalized :: (String -> String) -> [Path] -> AttrSet
  */
  importAllNormalized = norm: list: mapAttrs'
    (n: v: nameValuePair (norm n) v)
    (importAll list);

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: mapAttrsToList
    (n: t: toString (dir + "/${n}"))
    (filterAttrs pred (readDir dir));

  /* Predications used for `listDir'. */
  isDirType = name: type: type == "directory";
  isFileType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: name == "default.nix";
  isNixFile = name: type: isNotDirType name type && match ".+\\.nix$" name != null;
  isBaseNameSupportedSystem = name: type: isSupportedSystem (baseNameNoExt name);
  isImportable = name: type: isDirType name type || isNixFile name type;

  isNotDirType = name: type: ! isDirType name type;
  isNotFileType = name: type: ! isFileType name type;
  isNotSymbolicType = name: type: ! isSymbolicType name type;
  isNotDefaultNix = name: type: ! isDefaultNix name type;
  isNotNixFile = name: type: ! isNixFile name type;
  isNotBaseNameSupportSystem = name: type: ! isBaseNameSupportedSystem name type;
  isNotImportable = name: type: ! isImportable name type;

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name:
    let
      b = baseNameOf name;
      m = match "(.+)(\\.[^.]+)$" b;
    in if null == m then b else elemAt m 0;

  /* Create an AttrSet of Nix expressions from the given directory.
     Each attribute name is the file base name without extension.

     Note that the exceptions are, a) if the file/directory name is defined
     in the `supportedSystems'; b) if it is `default.nix'.
     For those files/directories they will not be imported by this function.

     Type:
       listNonPlatformSpecific :: Path -> [Path]
  */
  listNonPlatformSpecific = listDir (n: t:
    isNotBaseNameSupportSystem n t
    && isNotDefaultNix n t
    && isImportable n t);

  /* Similar with `listNonPlatformSpecific' this only lists files/folders for
     supported systems.

     Type:
       listPlatformSpecific :: Path -> String -> [Path]
  */
  listPlatformSpecific = node: system:
    assert isSupportedSystem system;
    let
      dir = node + "/${system}";
      file = dir + ".nix";
      get = p: optional (pathExists p) p;
    in get dir ++ get file;
}
