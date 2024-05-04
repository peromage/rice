{ self, nixpkgs, ... }:

let
  inherit (nixpkgs) foldl';
  inherit (builtints) head tail;

in with self; {
  /* Join a list of strings/paths with separaters.

     Type:
       join :: String -> [a] -> a
  */
  join = sep: list: foldl' (a: i: a + "${sep}${i}") (head list) (tail list);

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
      m = match "(.*)\\.[^.]+$" b;
    in if null == m then b else elemAt m 0;
}
